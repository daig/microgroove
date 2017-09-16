{-# language MagicHash #-}
module Data.Microgroove.Lib where
import Unsafe.Coerce (unsafeCoerce)

cast# :: forall b a. a -> b
cast# = unsafeCoerce
castf# :: forall g f x. f x -> g x
castf# = unsafeCoerce
overcast# :: forall b a. (b -> b) -> a -> a
overcast# = unsafeCoerce
mapCast# :: forall b f a. f a -> f b
mapCast# = unsafeCoerce

newtype Id a = Id a deriving Show
instance Functor Id where fmap f (Id a) = Id (f a)

newtype K a b = K a deriving Show
