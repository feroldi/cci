#pragma once
#include "cci/ast/arena_types.hpp"
#include "cci/ast/qual_type.hpp"
#include "cci/langopts.hpp"
#include <cstdint>
#include <memory_resource>
#include <new>

namespace cci::ast
{

// Side-table and resource manager of the AST.
struct ASTContext
{
    const TargetInfo &target_info;

    ASTContext(const TargetInfo &target) : target_info(target)
    {
        init_builtin_types();
    }

    auto allocate(size_t bytes,
                  size_t alignment = alignof(std::max_align_t)) const -> void *
    {
        return arena_resource.allocate(bytes, alignment);
    }

    template <typename T>
    auto allocate(size_t num = 1u) const -> T *
    {
        return static_cast<T *>(
            arena_resource.allocate(num * sizeof(T), alignof(T)));
    }

public:
    // Builtin C types. These are all canonical forms of the primitive/builtin
    // types. They are allocated in the arena memory resource when ASTContext is
    // constructed.
    QualType void_ty;
    QualType bool_ty;
    QualType char_ty;
    QualType schar_ty;
    QualType uchar_ty;
    QualType wchar_ty;
    QualType char16_t_ty;
    QualType char32_t_ty;
    QualType short_ty;
    QualType ushort_ty;
    QualType int_ty;
    QualType uint_ty;
    QualType long_ty;
    QualType ulong_ty;
    QualType long_long_ty;
    QualType ulong_long_ty;
    QualType float_ty;
    QualType double_ty;
    QualType long_double_ty;

private:
    // Arena memory resource used to create AST objects.
    //
    // AST objects are constructed here, but never destructed. All memory
    // associated with these AST objects will be released when the ASTContext
    // instance is itself destructed.
    //
    // This is mutable because ASTContext is passed around as a constant
    // reference.
    mutable std::pmr::monotonic_buffer_resource arena_resource;

    void init_builtin_types();
};

} // namespace cci::ast

// Placement new for construction of AST objects using the ASTContext's arena
// memory resource.
//
// Note: this doesn't throw unless the upstream resource of ASTContext's arena
// throws.
//
// Memory allocated with this placement new doesn't need to be explicitly freed,
// as it is obtained from ASTContext's arena memory resource, which will free
// all of its memory at destruction. DO NOT call `delete` on the returned
// pointer.
//
// Alignment is optionally passed to this overload. The default is always
// `alignof(std::max_align_t)`.
//
// Example of usage:
//
//     ASTContext context(...);
//     // Default alignment.
//     auto e1 = new (context) IntegerLiteral(...);
//     // Custom alignment.
//     auto e2 = new (context, 4) IntegerLiteral(...);
[[nodiscard]] inline void *
operator new(std::size_t bytes, const cci::ast::ASTContext &c,
             std::size_t alignment = alignof(std::max_align_t))
{
    return c.allocate(bytes, alignment);
}

// Placement new[] for construction of AST objects using the ASTContext's arena
// memory resource. Same rules of the new placement also apply.
//
// Note: this doesn't throw unless the upstream resource of ASTContext's arena
// throws.
//
// Example of usage:
//
//     ASTContext context(...);
//
//     // Default alignment.
//     auto locs = new (context) ByteLoc[length];
//
//     // Custom alignment.
//     auto data = new (context, 4) std::byte[4096];
[[nodiscard]] inline void *
operator new[](std::size_t bytes, const cci::ast::ASTContext &c,
               std::size_t alignment = alignof(std::max_align_t))
{
    return c.allocate(bytes, alignment);
}
