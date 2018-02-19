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
  ,toMVector, ctoMVector
  ,modify
  ,module X
  ) where
import Data.Microgroove.Lib
import qualified Data.Vector.Mutable as VM
import Control.Monad.Primitive (PrimMonad(..))
import Data.Microgroove.Mutable.Type as X



-- | Modify a mutable record in place by mapping a natural tranformation.
-- O(n)
rmap :: forall g m f xs. PrimMonad m => (forall x. f x -> g x) -> MRec (PrimState m) f xs -> m (MRec (PrimState m) g xs)
rmap f xs = cast# xs <$ go xs where
  go :: MRec (PrimState m) f as -> m ()
  go = \case
    MRNil -> pure ()
    MRCons x xs' -> VM.modify x (castf# @f . f) 0 >> go xs'
    _ -> error "impossible! MRNil and MRCons were inexhaustive in rmap"

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
    _ -> error "impossible! MRNil and MRCons were inexhaustive in crmap"

-- | Convert a mutable record to a mutable vector by mapping to a homogeneous type
-- O(n)
toMVector :: forall r m f xs. PrimMonad m
         => (forall x. f x -> r) -> MRec (PrimState m) f xs -> m (MVector (PrimState m) r)
toMVector f xs = cast# xs <$ go xs where
  go :: MRec (PrimState m) f as -> m ()
  go = \case
    MRNil -> pure ()
    MRCons x xs' -> VM.modify x (cast# . f) 0 >> go xs'
    _ -> error "impossible! MRNil and MRCons were inexhaustive in toMVector"

-- | Convert a mutable record to a mutable vector by mapping to a homogeneous type, making use of provided constraint
-- O(n)
ctoMVector :: forall (c :: * -> Constraint) r m f xs. (AllF c f xs, PrimMonad m)
          => (forall x. c (f x) => f x -> r) -> MRec (PrimState m) f xs -> m (MVector (PrimState m) r)
ctoMVector f xs = cast# xs <$ go xs where
  go :: forall as. (AllF c f as) => MRec (PrimState m) f as -> m ()
  go = \case
    MRNil -> pure ()
    MRCons x xs' -> VM.modify x (cast# . f) 0 >> go xs'
    _ -> error "impossible! MRNil and MRCons were inexhaustive in ctoMVector"

-- | Create a mutable record of the given shape. The memory is not initialized
new# :: forall f xs m. (KnownNat (Length xs), PrimMonad m) => m (MRec (PrimState m) f xs)
new# = MRec# <$> VM.unsafeNew (intVal @(Length xs))
-- | Modify a record in place by appling an endofunctor at the index. O(1)
modify :: forall n m f xs y. (KnownNat n, PrimMonad m)
       => (f (xs !! n) -> f y) -> MRec (PrimState m) f xs
       -> m (MRec (PrimState m) f (SetAt n xs y))
modify f rm@(MRec# vm) = cast# rm <$ VM.modify vm (cast# @Any . f . cast#) (intVal @n)
