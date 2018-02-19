module Data.Microgroove.Lib.Exists where

-- | The Existential Type @Some f@ is some @f x@ where @x@ is known at runtime
data Some f where Some :: f x -> Some f

-- | Avoids one indirection compared with @Maybe (Some f)@
data MaybeSome f = forall x. JustSome (f x) | None
