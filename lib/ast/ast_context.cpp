#include "cci/ast/ast_context.hpp"
#include "cci/ast/type.hpp"

namespace cci {

void ASTContext::init_builtin_types()
{
    auto make_builtin = [this](BuiltinTypeKind kind) {
        return QualType(BuiltinType::create(*this, kind), Qualifiers::None);
    };

    void_ty = make_builtin(BuiltinTypeKind::Void);
    bool_ty = make_builtin(BuiltinTypeKind::Bool);
    char_ty = make_builtin(BuiltinTypeKind::Char);
    schar_ty = make_builtin(BuiltinTypeKind::SChar);
    uchar_ty = make_builtin(BuiltinTypeKind::UChar);
    wchar_ty = make_builtin(BuiltinTypeKind::WChar);
    char16_t_ty = make_builtin(BuiltinTypeKind::Char16);
    char32_t_ty = make_builtin(BuiltinTypeKind::Char32);
    short_ty = make_builtin(BuiltinTypeKind::Short);
    ushort_ty = make_builtin(BuiltinTypeKind::UShort);
    int_ty = make_builtin(BuiltinTypeKind::Int);
    uint_ty = make_builtin(BuiltinTypeKind::UInt);
    long_ty = make_builtin(BuiltinTypeKind::Long);
    ulong_ty = make_builtin(BuiltinTypeKind::ULong);
    long_long_ty = make_builtin(BuiltinTypeKind::LongLong);
    ulong_long_ty = make_builtin(BuiltinTypeKind::ULongLong);
    float_ty = make_builtin(BuiltinTypeKind::Float);
    double_ty = make_builtin(BuiltinTypeKind::Double);
    long_double_ty = make_builtin(BuiltinTypeKind::LongDouble);
}

} // namespace cci
