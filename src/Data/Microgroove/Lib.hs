{-# language MagicHash #-}
module Data.Microgroove.Lib where
import Unsafe.Coerce (unsafeCoerce)

cast# :: forall b a. a -> b
cast# = unsafeCoerce
castf# :: forall g f x. f x -> g x
castf# = unsafeCoerce
overcast# :: forall b a. (b -> b) -> a -> a
overcast# = unsafeCoerce

newtype Id a = Id a deriving Show
instance Functor Id where fmap f (Id a) = Id (f a)

data Some f = forall x. Some (f x)
data MaybeSome f = forall x. JustSome (f x) | None

newtype K a b = K a deriving Show
