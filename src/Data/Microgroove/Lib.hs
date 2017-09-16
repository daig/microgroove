{-# language MagicHash #-}
module Data.Microgroove.Lib where
import Unsafe.Coerce (unsafeCoerce)

-- | @unsafeCoerce@ with the type arguments flipped for easier TypeApplications
cast# :: forall b a. a -> b
cast# = unsafeCoerce
-- | @unsafeCoerce@ as a natural transformation, keeping the inner type fixed
castf# :: forall g f x. f x -> g x
castf# = unsafeCoerce
-- | @unsafeCoerce@ an endofunction to operate on a different type
overcast# :: forall b a. (b -> b) -> a -> a
overcast# = unsafeCoerce
-- | equivalent to @fmap cast#@ but avoids traversing the datastructure
mapCast# :: forall b f a. f a -> f b
mapCast# = unsafeCoerce

-- | The Identity Functor
newtype Id a = Id a deriving Show
instance Functor Id where fmap f (Id a) = Id (f a)

-- | The Konstant Functor
newtype K a b = K a deriving Show
