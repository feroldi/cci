#pragma once

#include <memory>
#include <vector>
#include "cpp/any.hpp"
#include "cpp/observer_ptr.hpp"
#include "lexer.hpp"
#include "source_manager.hpp"
#include "program.hpp"

namespace ccompiler
{

enum class NodeType
{
  None,
  PrimaryExpression,
  GenericSelection,
  GenericAssocList,
  GenericAssociation,
  PostfixExpression,
  ArgumentExpressionList,
  UnaryExpression,
  UnaryOperator,
  CastExpression,
  MultiplicativeExpression,
  AdditiveExpression,
  ShiftExpression,
  RelatioalExpression,
  EqualityExpression,
  EqualityOperator,
  AndExpression,
  ExclusiveOrExpression,
  InclusiveOrExpression,
  LogicalAndExpression,
  LogicalOrExpression,
  ConditionalExpression,
  AssignmentExpression,
  AssignmentOperator,
  Expression,
  ConstantExpression,
  Declaration,
  DeclarationSpecifiers,
  DeclarationSpecifier,
  InitDeclaratorList,
  InitDeclarator,
  StorageClassSpecifier,
  TypeSpecifier,
  StructOrUnionSpecifier,
  StructOrUnion,
  StructDeclarationList,
  StructDeclaration,
  SpecifierQualifierList,
  StructDeclaratorList,
  StructDeclarator,
  EnumSpecifier,
  EnumeratorList,
  Enumerator,
  AtomicTypeSpecifier,
  TypeQualifier,
  FunctionSpecifier,
  AlignmentSpecifier,
  Declarator,
  DirectDeclarator,
  NestedParenthesesBlock,
  Pointer,
  TypeQualifierList,
  ParameterTypeList,
  ParameterList,
  ParameterDeclaration,
  IdentifierList,
  TypeName,
  AbstractDeclarator,
  DirectAbstractDeclarator,
  TypedefName,
  Initializer,
  InitializerList,
  Designation,
  DesignatorList,
  Designator,
  StaticAssertDeclaration,
  Statement,
  LabeledStatement,
  CompoundStatement,
  BlockItemList,
  BlockItem,
  ExpressionStatement,
  SelectionStatement,
  IterationStatement,
  JumpStatement,
  CompilationUnit,
  TranslationUnit,
  ExternalDeclaration,
  FunctionDefinition,
  DeclarationList,
  Identifier,
  Constant,
  IntegerConstant,
  FloatingConstant,
  EnumerationConstant,
  CharacterConstant,
  StringLiteral,
  StringLiteralList,
  AsmBlock,
};

auto to_string(const NodeType node_type) -> const char*;

struct SyntaxTree
{
  using iterator = std::vector<std::unique_ptr<SyntaxTree>>::iterator;
  using const_iterator = std::vector<std::unique_ptr<SyntaxTree>>::const_iterator;

  static auto parse(ProgramContext&, const TokenStream&) -> std::unique_ptr<SyntaxTree>;

  explicit SyntaxTree() noexcept = default;

  explicit SyntaxTree(NodeType type) noexcept
    : node_type(type), token(nullopt)
  {}

  explicit SyntaxTree(NodeType type, const TokenStream::TokenData& token) noexcept
    : node_type(type), token(token)
  {}

  SyntaxTree(SyntaxTree&& other) noexcept
    : node_type(other.node_type),
      node_parent(other.node_parent),
      token(std::move(other.token)),
      data(std::move(other.data))
  {
    other.node_type = NodeType::None;
    other.node_parent.reset();
  }

  SyntaxTree& operator= (SyntaxTree&&) noexcept = default;

  void add_child(std::unique_ptr<SyntaxTree> tree)
  {
    Expects(tree->parent() == nullptr);

    tree->node_parent.reset(this);
    this->children.emplace_back(std::move(tree));
  }

  void take_children(std::unique_ptr<SyntaxTree> tree)
  {
    this->children.reserve(this->child_count() + tree->child_count());

    for (auto& child : *tree)
    {
      child->node_parent.reset(this);
      this->children.emplace_back(std::move(child));
    }
  }

  auto child(size_t idx) -> std::unique_ptr<SyntaxTree>&
  {
    return this->children[idx];
  }

  auto parent() const noexcept -> observer_ptr<SyntaxTree>
  {
    return this->node_parent;
  }

  auto type() const noexcept -> NodeType
  {
    return this->node_type;
  }

  auto has_text() const noexcept -> bool
  {
    return this->token != nullopt;
  }

  auto text() const -> string_view
  {
    return static_cast<string_view>(this->token.value().data);
  }

  auto begin() noexcept -> iterator
  {
    return this->children.begin();
  }

  auto end() noexcept -> iterator
  {
    return this->children.end();
  }

  auto begin() const noexcept -> const_iterator
  {
    return this->children.begin();
  }

  auto end() const noexcept -> const_iterator
  {
    return this->children.end();
  }

  auto child_count() const noexcept -> std::size_t
  {
    return this->children.size();
  }

  template <typename F>
  void traverse(const F& f)
  {
    f(*this);

    for (auto& child : this->children)
    {
      child->traverse(f);
    }
  }

  void dump(std::FILE* out = stderr, size_t indent_level = 0) const;

private:
  NodeType node_type;
  observer_ptr<SyntaxTree> node_parent;
  std::vector<std::unique_ptr<SyntaxTree>> children;
  optional<TokenStream::TokenData> token;
  any data;
};

} // namespace ccompiler
