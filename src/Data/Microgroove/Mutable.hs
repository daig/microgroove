{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language UndecidableSuperClasses #-}
{-# language UndecidableInstances #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove.Mutable (
  -- * Mutable Heterogeneous Records
   MRec(MRec#,MRNil,MRCons)
  -- ** Constructing Records
  ,new#
  -- ** Indexing
  ,index
  -- ** Modifying Records
  ,rmap, rmapM
  ,crmap, crmapM
  -- *** Modifying Individual records
  ,modify_, cmodify_, modify
  -- ** Combining Records
  ,rzip, crzip
  -- ** Deconstructing Records
  ,toMVector, ctoMVector
  -- ** Filtering Records
  ,subRecord#
  ,module X
  ) where
import Data.Microgroove.Lib
import Data.Microgroove.Lib.Vector
import qualified Data.Vector.Mutable as VM
import Data.Vector.Mutable as X (MVector)
import Control.Monad.Primitive as X (PrimMonad(..))
import GHC.Exts as X (RealWorld)

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

-- | Match a nonempty record, refining its type.
-- The head is vector of length one to preserve mutable identity.
-- O(1)
pattern MRCons :: () => (us' ~ (u ': us)) => VM.MVector s (f u) -> MRec s f us -> MRec s f us'
pattern MRCons x xs <- (upMRec -> MRCons' x xs)

{-# complete MRNil, MRCons #-}




-- | Modify a mutable record in place by mapping a natural tranformation.
-- O(n)
rmap :: forall g m f xs. PrimMonad m => (forall x. f x -> g x) -> MRec (PrimState m) f xs -> m (MRec (PrimState m) g xs)
rmap f xs = cast# xs <$ go xs where
  go :: MRec (PrimState m) f as -> m ()
  go = \case
    MRNil -> pure ()
    MRCons x xs' -> VM.modify x (castf# @f . f) 0 >> go xs'

-- | Traverse a mutable record in place by mapping an effectful constrained tranformation.
-- O(n)
rmapM :: forall g m f xs. PrimMonad m => (forall x. f x -> m (g x)) -> MRec (PrimState m) f xs -> m (MRec (PrimState m) g xs)
rmapM f xs = cast# xs <$ go xs where
  go :: MRec (PrimState m) f as -> m ()
  go = \case
    MRNil -> pure ()
    MRCons x xs' -> do
      x' <- f =<< VM.unsafeRead x 0
      VM.write x 0 (castf# @f x')
      go xs'

-- | Traverse a mutable record in place by mapping an effectful natural tranformation.
-- O(n)
crmapM :: forall (c :: * -> Constraint) g m f xs. (AllF c f xs, PrimMonad m)
       => (forall x. c (f x) => f x -> m (g x)) -> MRec (PrimState m) f xs
       -> m (MRec (PrimState m) g xs)
crmapM f xs = cast# xs <$ go xs where
  go :: AllF c f as => MRec (PrimState m) f as -> m ()
  go = \case
    MRNil -> pure ()
    MRCons x xs' -> do
      x' <- f =<< VM.unsafeRead x 0
      VM.write x 0 (castf# @f x')
      go xs'

-- | Combine two mutable records elementwise with a natural combiner, _into_ the second the second record.
--
-- Mutates only the second argument. O(n)
rzip :: forall h m (f :: k -> *) g (xs :: [k]). PrimMonad m
     => (forall x. f x -> g x -> h x)
     -> MRec (PrimState m) f xs -> MRec (PrimState m) g xs
     -> m (MRec (PrimState m) h xs)
rzip f xs ys = cast# ys <$ go xs ys where
  go :: MRec (PrimState m) f as -> MRec (PrimState m) g as -> m ()
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
  go :: AllF c f as => MRec (PrimState m) f as -> m ()
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
  go :: (AllF c f as, AllF c g as)
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
  go :: AllF c f as => MRec (PrimState m) f as -> m ()
  go = \case
    MRNil -> pure ()
    MRCons x xs' -> VM.modify x (cast# . f) 0 >> go xs'

-- | Create a mutable record of the given shape. The memory is not initialized
new# :: forall f xs m. (KnownNat (Length xs), PrimMonad m) => m (MRec (PrimState m) f xs)
new# = MRec# <$> VM.unsafeNew (intVal @(Length xs))

-- | Modify a record in place by applying a natural transformation at the index. O(1)
modify_ :: forall n m f xs. (KnownNat n, PrimMonad m)
       => (forall x. f x -> f x) -> MRec (PrimState m) f xs
       -> m ()
modify_ f (MRec# vm) = VM.modify vm (cast# @Any . f . cast#) (intVal @n)

-- | Modify a record in place by applying a constrained transformation at the index. O(1)
cmodify_ :: forall (c :: * -> Constraint) n m f xs. (c (f (xs!!n)), KnownNat n, PrimMonad m)
       => (forall x. c (f x) => f x -> f x) -> MRec (PrimState m) f xs
       -> m ()
cmodify_ f (MRec# vm) = VM.modify vm (cast# @Any . f . cast# @(f (xs !! n))) (intVal @n)

-- | Modify a record in place by applying a function at the index. O(1)
modify :: forall n m f xs y. (KnownNat n, PrimMonad m)
       => (f (xs !! n) -> f y) -> MRec (PrimState m) f xs
       -> m (MRec (PrimState m) f (SetAt n xs y))
modify f rm@(MRec# vm) = cast# rm <$ VM.modify vm (cast# @Any . f . cast#) (intVal @n)

-- | Choose a satically known ordered subset of the fields in a record.
-- The list must be in ascending order. O(n)
subRecord# :: forall ns m f xs. (KnownNat (Length ns), KnownNats ns,PrimMonad m)
          => MRec (PrimState m) f xs -> m (MRec (PrimState m) f (SubList# ns xs))
subRecord# (MRec# vm) = MRec# <$> subVector# (intVal @(Length ns)) (intList @ns) vm
