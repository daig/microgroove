{-# language MagicHash #-}
module Data.Microgroove.Lib.Cast where
import Unsafe.Coerce

-- | @unsafeCoerce@ with the type arguments flipped for easier TypeApplications
cast# :: forall b a. a -> b
cast# = unsafeCoerce
-- | @unsafeCoerce@ as a natural transformation, keeping the inner type fixed
castf# :: forall g f x. f x -> g x
castf# = unsafeCoerce
-- | equivalent to @fmap cast#@ but avoids traversing the datastructure
mapCast# :: forall b f a. f a -> f b
mapCast# = unsafeCoerce
