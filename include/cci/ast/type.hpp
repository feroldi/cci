#pragma once
#include "cci/ast/arena_types.hpp"
#include "cci/syntax/source_map.hpp"
#include <cstdint>
#include <type_traits>

namespace cci {
struct ASTContext;

struct Qualifiers
{
private:
    uint32_t mask;

public:
    enum Qual
    {
        None = 0,
        Const = 1 << 0,
        Volatile = 1 << 1,
        Restrict = 1 << 2,
    };

    static auto from_mask(uint32_t mask) -> Qualifiers
    {
        Qualifiers q;
        q.mask = mask;
        return q;
    }

    bool has_const() const { return mask & Const; }
    void add_const() { mask |= Const; }
    void clear_const() { mask &= ~Const; }
    void set_const(bool flag) { flag ? add_const() : clear_const(); }

    bool has_volatile() const { return mask & Volatile; }
    void add_volatile() { mask |= Volatile; }
    void clear_volatile() { mask &= ~Volatile; }
    void set_volatile(bool flag) { flag ? add_volatile() : clear_volatile(); }

    bool has_restrict() const { return mask & Restrict; }
    void add_restrict() { mask |= Restrict; }
    void clear_restrict() { mask &= ~Restrict; }
    void set_restrict(bool flag) { flag ? add_restrict() : clear_restrict(); }

    bool has_qualifiers() const { return mask; }
};

enum class TypeClass
{
    Builtin,
    ConstantArray,
    Pointer,
};

struct Type
{
    TypeClass type_class;

    Type(const Type &) = delete;
    Type &operator=(const Type &) = delete;

    bool is_array_type() const;
    bool is_integer_type() const;

    template <typename T>
    auto get_as() const -> arena_ptr<const T>
    {
        return T::classof(type_class) ? static_cast<arena_ptr<const T>>(this)
                                      : nullptr;
    }

protected:
    friend struct ASTContext;
    explicit Type(TypeClass tc) : type_class(tc) {}
};

struct QualType
{
private:
    arena_ptr<Type> type;

public:
    Qualifiers qualifiers;

    QualType() = default;
    QualType(arena_ptr<Type> ty, uint32_t quals_mask)
        : type(ty), qualifiers(Qualifiers::from_mask(quals_mask))
    {}
    QualType(arena_ptr<Type> ty, Qualifiers quals) : type(ty), qualifiers(quals)
    {}

    explicit operator bool() const noexcept { return type; }
    auto operator*() const noexcept -> const Type & { return *type; }
    auto operator-> () const noexcept { return type; }
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
    BuiltinTypeKind builtin_kind;

    BuiltinType(BuiltinTypeKind k) : Type(TypeClass::Builtin), builtin_kind(k)
    {}

    static bool classof(TypeClass tc) { return TypeClass::Builtin == tc; }
};

struct ArrayType : Type
{
    QualType elem_type;

    ArrayType(TypeClass tc, QualType et) : Type(tc), elem_type(et) {}

    static bool classof(TypeClass tc) { return TypeClass::ConstantArray == tc; }
};

struct ConstantArrayType : ArrayType
{
    uint64_t length;

    ConstantArrayType(QualType elem_ty, uint64_t len)
        : ArrayType(TypeClass::ConstantArray, elem_ty), length(len)
    {}

    static bool classof(TypeClass tc) { return TypeClass::ConstantArray == tc; }
};

struct PointerType : Type
{
    QualType pointee_type;
    srcmap::ByteLoc star_loc;

    PointerType(QualType pointee_ty, srcmap::ByteLoc star_loc)
        : Type(TypeClass::Pointer), pointee_type(pointee_ty), star_loc(star_loc)
    {}

    static bool classof(TypeClass tc) { return TypeClass::Pointer == tc; }
};

inline bool Type::is_array_type() const
{
    return type_class == TypeClass::ConstantArray;
}

inline bool Type::is_integer_type() const
{
    if (auto bt = get_as<BuiltinType>())
    {
        return bt->builtin_kind >= BuiltinTypeKind::Bool &&
               bt->builtin_kind <= BuiltinTypeKind::ULongLong;
    }
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
