#pragma once

namespace cci {

/// A pointer to a trivially destructible object in an arena.
///
/// An object encapsulated in such a pointer does not need a destructor call,
/// and may be disposed of by simply deallocating its storage. Its purpose is to
/// guarantee the destructor triviality of objects that compound a node in the
/// AST. If, for any reason, a type changes the traits of its destructor to be
/// nontrivial, this will therefore prevent compilation.
template <typename T>
// FIXME: std::enable_if_t<std::is_trivially_destructible_v<T>, int> = 0>
using arena_ptr = T *;

} // namespace cci
