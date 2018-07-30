#pragma once
#include "cci/ast/arena_types.hpp"
#include <cstdint>
#include <memory>

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
};

struct QualType
{
private:
  arena_ptr<Type> type;
  Qualifiers quals;

public:
  QualType() = default;
  QualType(arena_ptr<Type> ty, uint32_t quals_mask)
    : type(ty), quals(Qualifiers::from_mask(quals_mask))
  {}
  QualType(arena_ptr<Type> ty, Qualifiers quals) : type(ty), quals(quals) {}

  auto qualifiers() const -> Qualifiers { return quals; }

  explicit operator bool() const noexcept { return type; }
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
private:
  BuiltinTypeKind kind;

public:
  BuiltinType(BuiltinTypeKind k) : Type(TypeClass::Builtin), kind(k) {}

  auto type_kind() const -> BuiltinTypeKind { return kind; }
};

struct ArrayType : Type
{
private:
  QualType elem_type;

public:
  ArrayType(TypeClass tc, QualType et) : Type(tc), elem_type(et) {}

  auto element_type() const -> const QualType & { return elem_type; }
};

struct ConstantArrayType : ArrayType
{
private:
  uint64_t size_;

public:
  ConstantArrayType(QualType elem_ty, uint64_t sz)
    : ArrayType(TypeClass::ConstantArray, elem_ty), size_(sz)
  {}

  auto size() const -> uint64_t { return size_; }
};

} // namespace cci
