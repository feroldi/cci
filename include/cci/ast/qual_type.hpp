#pragma once
#include "cci/ast/arena_types.hpp"
#include <cstdint>

namespace cci {
struct Type;

struct Qualifiers
{
private:
    uint8_t mask;

public:
    enum Qual : uint8_t
    {
        None = 0,
        Const = 1 << 0,
        Volatile = 1 << 1,
        Restrict = 1 << 2,
    };

    static auto from_mask(uint8_t mask) -> Qualifiers
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
    bool empty() const { return !mask; }
};

struct QualType
{
private:
    arena_ptr<Type> type;

public:
    Qualifiers qualifiers;

    QualType() = default;
    QualType(arena_ptr<Type> ty, uint8_t quals_mask)
        : type(ty), qualifiers(Qualifiers::from_mask(quals_mask))
    {}
    QualType(arena_ptr<Type> ty, Qualifiers quals) : type(ty), qualifiers(quals)
    {}

    explicit operator bool() const noexcept { return type; }
    auto operator*() const noexcept -> const Type & { return *type; }
    auto operator-> () const noexcept { return type; }
};
} // namespace cci
