#pragma once
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/qual_type.hpp"
#include <cstdint>
#include <type_traits>

namespace cci {
enum class TypeClass
{
    Builtin,
    ConstantArray,
    Pointer,
    Atomic,
};

struct Type
{
private:
    TypeClass tc;

protected:
    friend struct ASTContext;
    explicit Type(TypeClass tc) : tc(tc) {}

public:
    Type(const Type &) = delete;
    Type &operator=(const Type &) = delete;

    auto type_class() const -> TypeClass { return tc; }

    bool is_array_type() const;
    bool is_integer_type() const;
    bool is_void_type() const;

    template <typename T>
    auto get_as() const -> const T *
    {
        return T::classof(tc) ? static_cast<const T *>(this) : nullptr;
    }
};

enum class BuiltinTypeKind
{
    Void,
    Bool,
    Char,
    SChar,
    UChar,
    WChar,
    Char16,
    Char32,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    Float,
    Double,
    LongDouble,
};

// Builtin types.
struct BuiltinType : Type
{
private:
    friend struct ASTContext;
    BuiltinTypeKind btk;
    BuiltinType(BuiltinTypeKind btk) : Type(TypeClass::Builtin), btk(btk) {}

public:
    auto builtin_kind() const -> BuiltinTypeKind { return btk; }

    static auto create(const ASTContext &ctx, BuiltinTypeKind btk)
        -> arena_ptr<BuiltinType>
    {
        return new (ctx) BuiltinType(btk);
    }

    static bool classof(TypeClass tc) { return TypeClass::Builtin == tc; }
};

struct ArrayType : Type
{
private:
    QualType elem_ty;

protected:
    friend struct ASTContext;
    ArrayType(TypeClass tc, QualType et) : Type(tc), elem_ty(et) {}

public:
    auto element_type() const -> QualType { return elem_ty; }

    static bool classof(TypeClass tc) { return TypeClass::ConstantArray == tc; }
};

struct ConstantArrayType : ArrayType
{
private:
    uint64_t len;

    ConstantArrayType(QualType elem_ty, uint64_t len)
        : ArrayType(TypeClass::ConstantArray, elem_ty), len(len)
    {}

public:
    auto array_length() const -> uint64_t { return len; }

    static auto create(const ASTContext &ctx, QualType element_type,
                       uint64_t length) -> arena_ptr<ConstantArrayType>
    {
        return new (ctx) ConstantArrayType(element_type, length);
    }

    static bool classof(TypeClass tc) { return TypeClass::ConstantArray == tc; }
};

struct PointerType : Type
{
private:
    QualType pointee_ty;

    PointerType(QualType pointee_ty)
        : Type(TypeClass::Pointer), pointee_ty(pointee_ty)
    {}

public:
    static auto create(const ASTContext &ctx, QualType pointee_type)
        -> arena_ptr<PointerType>
    {
        return new (ctx) PointerType(pointee_type);
    }

    static bool classof(TypeClass tc) { return TypeClass::Pointer == tc; }

    auto pointee_type() const -> QualType { return pointee_ty; }
};

struct AtomicType : Type
{
private:
    QualType value_ty;

    AtomicType(QualType value_ty) : Type(TypeClass::Atomic), value_ty(value_ty)
    {}

public:
    static auto create(const ASTContext &ctx, QualType value_type)
        -> arena_ptr<AtomicType>
    {
        return new (ctx) AtomicType(value_type);
    }

    auto value_type() const -> QualType { return value_ty; }
};

inline bool Type::is_array_type() const
{
    return TypeClass::ConstantArray == type_class();
}

inline bool Type::is_integer_type() const
{
    if (auto bt = get_as<BuiltinType>())
        return bt->builtin_kind() >= BuiltinTypeKind::Bool &&
               bt->builtin_kind() <= BuiltinTypeKind::ULongLong;
    return false;
}

inline bool Type::is_void_type() const
{
    if (auto bt = get_as<BuiltinType>())
        return bt->builtin_kind() == BuiltinTypeKind::Void;
    return false;
}

static_assert(std::is_trivially_destructible_v<Qualifiers>);
static_assert(std::is_trivially_destructible_v<Type>);
static_assert(std::is_trivially_destructible_v<QualType>);
static_assert(std::is_trivially_destructible_v<BuiltinType>);
static_assert(std::is_trivially_destructible_v<ArrayType>);
static_assert(std::is_trivially_destructible_v<ConstantArrayType>);
static_assert(std::is_trivially_destructible_v<PointerType>);

} // namespace cci
