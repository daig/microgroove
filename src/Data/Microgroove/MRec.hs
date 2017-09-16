{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language UndecidableSuperClasses #-}
{-# language UndecidableInstances #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove.MRec
  (MRec(MRec#,MRNil,MRCons)
  ,rmap, crmap
  ,toMVector, ctoMVector
  ) where
import Data.Microgroove.TypeLevel
import Data.Microgroove.Lib

import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as VM
import GHC.Exts (Any,Constraint)
import Control.Monad.Primitive (PrimMonad(..))


-- | A mutable heterogeneous record represented by an untyped mutable vector
newtype MRec s (f :: u -> *) (us :: [u]) = MRec# (MVector s Any)
-- | An intermediate type to deconstruct an @MRec@ into head normal form
data MRec' s (f :: u -> *) (us :: [u]) where
  MRNil' :: MRec' s f '[]
  MRCons' :: MVector s (f u) -> MRec s f us -> MRec' s f (u ': us)

-- | Split a mutable record into a head vector of length one, and the remaining record
-- must be statically known to be nonempty
-- O(1)
{-splitCons# :: MRec s f (x ': xs) -> (MVector s (f x),MRec s f xs)-}
{-splitCons# (MRec# v) = (cast# $ VM.take 1 v, MRec# $ VM.tail v)-}

-- | Convert an MRec to head normal form,
-- refining the type to distinguish empty from nonempty records
upMRec :: MRec s f us -> MRec' s f us
upMRec (MRec# v) | VM.null v = cast# MRNil'
                 | otherwise = cast# $ MRCons' (cast# $ VM.take 1 v) (MRec# $ VM.tail v)

-- | Match an empty record, refining its type
pattern MRNil :: () => (us ~ '[]) => MRec s f us
pattern MRNil <- (upMRec -> MRNil')

-- | Match a nonempty record, refining its type
-- The head is vector of length one to preserve mutable identity
-- O(1)
pattern MRCons :: () => (us' ~ (u ': us)) => VM.MVector s (f u) -> MRec s f us -> MRec s f us'
pattern MRCons x xs <- (upMRec -> MRCons' x xs)


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
