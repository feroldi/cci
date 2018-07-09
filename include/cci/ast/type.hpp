#pragma once
#include <cstdint>
#include <memory>

namespace cci {

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

public:
  explicit Type(TypeClass tc) : tc(tc) {}

  auto type_class() const -> TypeClass { return tc; }
};

struct QualifiedType
{
private:
  std::unique_ptr<Type> type;
  Qualifiers qualifiers;

public:
  QualifiedType() = default;
  QualifiedType(std::unique_ptr<Type> ty, uint32_t quals_mask)
    : type(std::move(ty)), qualifiers(Qualifiers::from_mask(quals_mask))
  {}
  QualifiedType(std::unique_ptr<Type> ty, Qualifiers quals)
    : type(std::move(ty)), qualifiers(quals)
  {}

  // FIXME: This is temporary. Do not depend on this. Will be removed once Types
  // are treated like raw pointers.
  auto clone() const -> QualifiedType
  {
    auto ty = std::make_unique<Type>(this->type->type_class());
    return QualifiedType(std::move(ty), this->qualifiers);
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
  BuiltinTypeKind kind;

public:
  BuiltinType(BuiltinTypeKind k) : Type(TypeClass::Builtin), kind(k) {}

  auto type_kind() const -> BuiltinTypeKind { return kind; }
};

struct ArrayType : Type
{
private:
  QualifiedType elem_type;

public:
  ArrayType(TypeClass tc, QualifiedType et) : Type(tc), elem_type(std::move(et))
  {}

  auto element_type() const -> const QualifiedType & { return elem_type; }
};

struct ConstantArrayType : ArrayType
{
private:
  uint64_t size_;

public:
  ConstantArrayType(QualifiedType elem_ty, uint64_t sz)
    : ArrayType(TypeClass::ConstantArray, std::move(elem_ty)), size_(sz)
  {}

  auto size() const -> uint64_t { return size_; }
};

} // namespace cci
