#pragma once

#include <memory>
#include <vector>
#include "lexer.hpp"
#include "source_manager.hpp"
#include "program.hpp"

namespace ccompiler
{

enum class NodeType
{
  TranslationUnit,
  Constant,
};

struct SyntaxTree : std::enable_shared_from_this<SyntaxTree>
{
  using iterator = std::vector<std::shared_ptr<SyntaxTree>>::iterator;
  using const_iterator = std::vector<std::shared_ptr<SyntaxTree>>::const_iterator;

  static auto parse(ProgramContext&, const TokenStream&) -> std::shared_ptr<SyntaxTree>;

  explicit SyntaxTree(NodeType type, TokenStream::iterator first,
                      TokenStream::iterator last)
    : node_type(type), token_range(first, last)
  {}

  explicit SyntaxTree(NodeType type, TokenStream::iterator it)
    : node_type(type), token_range(it, std::next(it))
  {}

  SyntaxTree(const SyntaxTree&) = delete;
  SyntaxTree(SyntaxTree&&) = default;

  SyntaxTree& operator= (const SyntaxTree&) = delete;
  SyntaxTree& operator= (SyntaxTree&&) = default;

  void add_child(std::shared_ptr<SyntaxTree> tree)
  {
    Expects(tree->node_parent == nullopt);
    tree->node_parent = std::weak_ptr<SyntaxTree>(this->shared_from_this());
    this->children.emplace_back(std::move(tree));
  }

  void take_children(std::vector<std::shared_ptr<SyntaxTree>> tree)
  {
    for (auto& child : tree)
    {
      child->node_parent = std::weak_ptr<SyntaxTree>(this->shared_from_this());
      this->children.emplace_back(std::move(child));
    }
  }

  auto parent() const noexcept -> std::shared_ptr<SyntaxTree>
  {
    if (this->node_parent != nullopt)
    {
      return this->node_parent->lock();
    }

    return nullptr;
  }

  struct TokenRange
  {
    TokenStream::iterator begin;
    TokenStream::iterator end;

    TokenRange(TokenStream::iterator begin, TokenStream::iterator end) noexcept
      : begin(begin), end(end)
    {}

    operator SourceRange() const noexcept
    {
      auto first_loc = this->begin->data.begin();
      auto last_loc = std::prev(this->end)->data.end();

      return SourceRange(first_loc, last_loc);
    }
  };

  auto type() const noexcept -> NodeType { return this->node_type; }

  auto tokens() const noexcept -> TokenRange { return this->token_range; }

  auto range() const noexcept -> SourceRange
  {
    return static_cast<SourceRange>(this->tokens());
  }

  auto begin() const noexcept -> const_iterator
  {
    return this->children.begin();
  }

  auto end() const noexcept -> const_iterator
  {
    return this->children.end();
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

private:
  NodeType node_type;
  optional<std::weak_ptr<SyntaxTree>> node_parent;
  std::vector<std::shared_ptr<SyntaxTree>> children;
  TokenRange token_range;
};

} // namespace
