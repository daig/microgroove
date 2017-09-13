# microgroove
Microgroove supports type-safe positional heterogeneous records similar to [vinyl](https://hackage.haskell.org/package/vinyl-0.6.0/docs/Data-Vinyl-Core.html#t:Rec) and [SOP](https://hackage.haskell.org/package/generics-sop-0.3.1.0/docs/Generics-SOP.html#t:NP).
Unlike these record types which are represented by linked lists, `microgroove`'s `Rec` type is backed by arrays, and so support constant-time indexing and mutable updates via the associated `MRec` type.

Microgroove can be used for lightweight statically specified polymorphic records just like `vinyl`, but also dynamic record types that are only provided at run-time, such as receiving an arbitrary JSON protocol.

# build
The recommended way to build microgroove is via [nix](https://nixos.org/nix/)
With [nix integration](https://cabal.readthedocs.io/en/latest/nix-integration.html) in `cabal-install` 2.0 or greater, that's just `cabal --enable-nix build`
or use [stack](https://www.haskellstack.org) via `stack build`
