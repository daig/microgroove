# microgroove
Microgroove supports type-safe positional heterogeneous records similar to [vinyl](https://hackage.haskell.org/package/vinyl-0.6.0/docs/Data-Vinyl-Core.html#t:Rec) and [SOP](https://hackage.haskell.org/package/generics-sop-0.3.1.0/docs/Generics-SOP.html#t:NP).

Microgroove can be used for lightweight statically specified polymorphic records just like `vinyl`, but also dynamic record types that are only provided at run-time, such as receiving an arbitrary JSON protocol.

# build
The recommended way to build microgroove is via [stack](https://www.haskellstack.org) with `stack build`

# contribute
Array-backed records have been merged into vinyl as `ARec`s! Please continue to contribute further development there.
