{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language UndecidableSuperClasses #-}
{-# language UndecidableInstances #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove.Mutable
  (MRec(MRec#,MRNil,MRCons)
  ,new#
  ,rmap, crmap
  ,rzip, crzip
  ,toMVector, ctoMVector
  ,modify, index
  ,module X
  ) where
import Data.Microgroove.Lib
import qualified Data.Vector.Mutable as VM
import Control.Monad.Primitive as X (PrimMonad(..))
import Data.Microgroove.Mutable.Type as X



-- | Modify a mutable record in place by mapping a natural tranformation.
-- O(n)
rmap :: forall g m f xs. PrimMonad m => (forall x. f x -> g x) -> MRec (PrimState m) f xs -> m (MRec (PrimState m) g xs)
rmap f xs = cast# xs <$ go xs where
  go :: MRec (PrimState m) f as -> m ()
  go = \case
    MRNil -> pure ()
    MRCons x xs' -> VM.modify x (castf# @f . f) 0 >> go xs'

-- | Combine two mutable records elementwise with a natural combiner, _into_ the second the second record.
--
-- Mutates only the second argument. O(n)
rzip :: forall h m (f :: k -> *) g (xs :: [k]). PrimMonad m
     => (forall x. f x -> g x -> h x)
     -> MRec (PrimState m) f xs -> MRec (PrimState m) g xs
     -> m (MRec (PrimState m) h xs)
rzip f xs ys = cast# ys <$ go xs ys where
  go :: forall as. MRec (PrimState m) f as -> MRec (PrimState m) g as -> m ()
  go MRNil MRNil = pure ()
  go (MRCons x xs') (MRCons y ys') = do
    x' <- VM.unsafeRead x 0
    VM.modify y (castf# @g . f x') 0
    go xs' ys'

index :: forall n m f xs. (KnownNat n,PrimMonad m) => MRec (PrimState m) f xs -> m (f (xs !! n))
index (MRec# vm) = mapCast# $ vm `VM.unsafeRead` intVal @n
  

-- | Modify a mutable record in place by mapping a natural tranformation that can make use of the provided constraint.
-- Ex: `crmap @Show (K . show) :: (MRec s f xs) -> ST s (MRec s (K String) xs)`
-- O(n)
crmap :: forall (c :: * -> Constraint) g m f xs. (AllF c f xs, PrimMonad m)
     => (forall x. c (f x) => f x -> g x) -> MRec (PrimState m) f xs -> m (MRec (PrimState m) g xs)
crmap f xs = cast# xs <$ go xs where
  go :: forall as. (AllF c f as) => MRec (PrimState m) f as -> m ()
  go = \case
    MRNil -> pure ()
    MRCons x xs' -> VM.modify x (castf# @f . f) 0 >> go xs'

-- | Combine two mutable records elementwise with a constrained combiner, _into_ the second the second record.
--
-- Mutates only the second argument. O(n)
crzip :: forall (c :: * -> Constraint) h m (f :: k -> *) g (xs :: [k])
      . (AllF c f xs, AllF c g xs, PrimMonad m)
     => (forall x. (c (f x), c (g x)) => f x -> g x -> h x)
     -> MRec (PrimState m) f xs -> MRec (PrimState m) g xs
     -> m (MRec (PrimState m) h xs)
crzip f xs ys = cast# ys <$ go xs ys where
  go :: forall as. (AllF c f as, AllF c g as)
     => MRec (PrimState m) f as -> MRec (PrimState m) g as -> m ()
  go MRNil MRNil = pure ()
  go (MRCons x xs') (MRCons y ys') = do
    x' <- VM.unsafeRead x 0
    VM.modify y (castf# @g . f x') 0
    go xs' ys'

-- | Convert a mutable record to a mutable vector by mapping to a homogeneous type
-- O(n)
toMVector :: forall r m f xs. PrimMonad m
         => (forall x. f x -> r) -> MRec (PrimState m) f xs -> m (MVector (PrimState m) r)
toMVector f xs = cast# xs <$ go xs where
  go :: MRec (PrimState m) f as -> m ()
  go = \case
    MRNil -> pure ()
    MRCons x xs' -> VM.modify x (cast# . f) 0 >> go xs'

-- | Convert a mutable record to a mutable vector by mapping to a homogeneous type, making use of provided constraint
-- O(n)
ctoMVector :: forall (c :: * -> Constraint) r m f xs. (AllF c f xs, PrimMonad m)
          => (forall x. c (f x) => f x -> r) -> MRec (PrimState m) f xs -> m (MVector (PrimState m) r)
ctoMVector f xs = cast# xs <$ go xs where
  go :: forall as. (AllF c f as) => MRec (PrimState m) f as -> m ()
  go = \case
    MRNil -> pure ()
    MRCons x xs' -> VM.modify x (cast# . f) 0 >> go xs'

-- | Create a mutable record of the given shape. The memory is not initialized
new# :: forall f xs m. (KnownNat (Length xs), PrimMonad m) => m (MRec (PrimState m) f xs)
new# = MRec# <$> VM.unsafeNew (intVal @(Length xs))
-- | Modify a record in place by appling an endofunctor at the index. O(1)
modify :: forall n m f xs y. (KnownNat n, PrimMonad m)
       => (f (xs !! n) -> f y) -> MRec (PrimState m) f xs
       -> m (MRec (PrimState m) f (SetAt n xs y))
modify f rm@(MRec# vm) = cast# rm <$ VM.modify vm (cast# @Any . f . cast#) (intVal @n)
