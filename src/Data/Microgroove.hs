{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# language MagicHash #-}
{-# language UndecidableInstances #-}
{-# language FlexibleContexts #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove
  (Rec(Rec#,RNil,(:&)), MRec(..)
  ,index, (!), checkIndex, checkIndex'
  , new, new'
  ,splitCons, rappend
  ,rmap, crmap, rmapM, crmapM
  ,rzip, crzip
  ,toVector, ctoVector
  ,foldMapF, cfoldMapF
  ,fromVectorN, fromVector, replicate
  ,thaw, thaw#, freeze, freeze#
  ,modify
  ,module X
  ) where
import Prelude hiding (replicate)
import Data.Microgroove.Lib
import Data.Microgroove.Lib.TypeLevel as X
import Data.Microgroove.Index as X
import Data.Microgroove.Type
import Data.Microgroove.Mutable (MRec(..))
import qualified Data.Microgroove.Mutable as M

import qualified Data.Vector as V
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad(..))
import GHC.TypeLits
import GHC.Float

-- | Copy a record into a fresh mutable record.
-- O(n)
thaw :: PrimMonad m => Rec f us -> m (MRec (PrimState m) f us)
thaw (Rec# v) = MRec# <$> V.thaw v
-- | unsafely thaw a record. The original record should no longer be used, but this is not checked.
-- O(1)
thaw# :: PrimMonad m => Rec f us -> m (MRec (PrimState m) f us)
thaw# (Rec# v) = MRec# <$> V.unsafeThaw v
-- | Copy a mutable record into a fresh record.
-- O(n)
freeze :: PrimMonad m => MRec (PrimState m) f us -> m (Rec f us)
freeze (MRec# v) = Rec# <$> V.freeze v
-- | Unsafely freeze a mutable record. The original record should no longer be modified, but this is not checked.
-- O(1)
freeze# :: PrimMonad m => MRec (PrimState m) f us -> m (Rec f us)
freeze# (MRec# v) = Rec# <$> V.unsafeFreeze v

-- | Split a record into a head element, and the remaining record
-- must be statically known to be nonempty.
-- O(1)
splitCons :: Rec f (x ': xs) -> (f x,Rec f xs)
splitCons (Rec# v) = (cast# $ V.head v, Rec# $ V.tail v)

  

-- | Index into a record with a prepared index.
-- O(1)
(!) :: Rec f us -> Index us u -> f u
Rec# v ! Index# i = cast# $ v V.! i

-- | Append two records.
-- O(n+m)
rappend :: Rec f as -> Rec f bs -> Rec f (as ++ bs)
rappend (Rec# as) (Rec# bs) = Rec# $ as V.++ bs

-- | Transform a record by mapping a natural tranformation over each element.
-- O(n)
rmap :: (forall x. f x -> g x) -> Rec f xs -> Rec g xs
rmap f r = runST $ freeze# =<< M.rmap f =<< thaw r

-- | Traverse a record by mapping an effectful natural tranformation over each element.
-- O(n)
rmapM :: forall m f g xs. PrimMonad m => (forall x. f x -> m (g x)) -> Rec f xs -> m (Rec g xs)
rmapM f r = freeze# =<< M.rmapM f =<< thaw r

-- | Traverse a record by mapping an effectful constrained tranformation.
-- O(n)
crmapM :: forall c m g f xs. (PrimMonad m, AllF c f xs)
      => (forall x. c (f x) => f x -> m (g x)) -> Rec f xs -> m (Rec g xs)
crmapM f r = freeze# =<< M.crmapM @c f =<< thaw r

-- | Transform a record by mapping a natural tranformation that can make use of the provided constraint. Ex:
--
-- > crmap @Show (K . show) :: (Rec f xs) -> (MRec (K String) xs)
--
-- O(n)
crmap :: forall c g f xs. AllF c f xs
      => (forall x. c (f x) => f x -> g x) -> Rec f xs -> Rec g xs
crmap f r = runST $ freeze# =<< M.crmap @c f =<< thaw r

-- | Zip two records together with a natural combiner. O(n)
rzip :: forall h (f :: k -> *) g (xs :: [k]). (forall x. f x -> g x -> h x)
     -> Rec f xs -> Rec g xs -> Rec h xs
rzip f fs gs = runST $ do
  fs' <- thaw# fs
  gs' <- thaw gs
  freeze# =<< M.rzip f fs' gs'

crzip :: forall (c :: * -> Constraint) h f g (xs :: [Type])
      . (AllF c f xs, AllF c g xs) => (forall x. (c (f x), c (g x)) => f x -> g x -> h x)
     -> Rec f xs -> Rec g xs -> Rec h xs
crzip = crzip' @Type @c
crzip' :: forall (c :: * -> Constraint) h f g (xs :: [k])
      . (AllF c f xs, AllF c g xs) => (forall x. (c (f x), c (g x)) => f x -> g x -> h x)
     -> Rec f xs -> Rec g xs -> Rec h xs
crzip' f fs gs = runST $ do
  fs' <- thaw fs
  gs' <- thaw gs
  freeze# =<< M.crzip @k @c f fs' gs'

-- | initialize each field with @mempty@, and @mappend@ fields pairwise
instance (KnownNat (Length xs), AllF Monoid f (xs :: [k])) => Monoid (Rec f xs) where
  mempty = new' @k @Monoid @xs mempty
  mappend = crzip' @k @Monoid mappend

instance (KnownNat (Length xs), AllF Num f (xs :: [k])) => Num (Rec f xs) where
  fromInteger n = new' @k @Num (fromInteger n)
  (+) = crzip' @k @Num (+)
  (-) = crzip' @k @Num (-)
  (*) = crzip' @k @Num (*)
  negate = crmap @Num negate
  abs = crmap @Num abs
  signum = crmap @Num signum

instance (KnownNat (Length xs), AllF Num f xs, AllF Fractional f (xs :: [k])) => Fractional (Rec f xs) where
  fromRational n = new' @k @Fractional (fromRational n)
  (/) = crzip' @k @Fractional (/)
  recip = crmap @Fractional recip

instance (KnownNat (Length xs), Fractional (Rec f (xs :: [k])), AllF Floating f xs) => Floating (Rec f xs) where
  pi = new' @k @Floating pi
  exp = crmap @Floating exp
  log = crmap @Floating log
  sqrt = crmap @Floating sqrt
  sin = crmap @Floating sin
  cos = crmap @Floating cos
  tan = crmap @Floating tan
  asin = crmap @Floating asin
  acos = crmap @Floating acos
  atan = crmap @Floating atan
  sinh = crmap @Floating sinh
  cosh = crmap @Floating cosh
  tanh = crmap @Floating tanh
  asinh = crmap @Floating asinh
  acosh = crmap @Floating acosh
  atanh = crmap @Floating atanh
  log1p = crmap @Floating log1p
  expm1 = crmap @Floating expm1
  log1pexp = crmap @Floating log1pexp
  log1mexp = crmap @Floating log1mexp




-- | Convert a record to a vector by mapping to a homogeneous type.
-- O(n)
toVector :: (forall x. f x -> r) -> Rec f xs -> Vector r
toVector f r = runST $ V.unsafeFreeze =<< M.toMVector f =<< thaw r

-- | Convert a record to a vector by mapping to a homogeneous type.
-- O(n)
{-ctoVector :: AllF c f xs => (forall x. c (f x) => f x -> r) -> Rec f xs -> Vector r-}
{-toVector f r = runST $ V.unsafeFreeze =<< M.toMVector f =<< thaw r-}

-- | Fold over a record by naturally mapping to a @Monoid@ accumulator
foldMapF :: forall r f xs. Monoid r => (forall x. f x -> r) -> Rec f xs -> r
foldMapF f = go where
  go :: forall as. Rec f as -> r
  go = \case
    RNil -> mempty
    x :& xs -> f x `mappend` go xs

-- | Fold over a record by mapping to a @Monoid@ accumulator
cfoldMapF :: forall (c :: * -> Constraint) r f xs. (AllF c f xs, Monoid r)
          => (forall x. c (f x) => f x -> r) -> Rec f xs -> r
cfoldMapF f = go where
  go :: forall as. AllF c f as => Rec f as -> r
  go = \case
    RNil -> mempty
    x :& xs -> f x `mappend` go xs

-- | Convert a record to a vector by mapping to a homogeneous type, making use of provided constraint.
-- O(n)
ctoVector :: forall c r f xs. AllF c f xs
         => (forall x. c (f x) => f x -> r) -> Rec f xs -> Vector r
ctoVector f r = runST $ V.unsafeFreeze =<< M.ctoMVector @c f =<< thaw r

-- | Create a record of statically known size by replicating a single element.
-- O(n)
replicate :: forall n f x. KnownNat n => f x -> Rec f (Replicate n x)
replicate = Rec# . mapCast# @Any . V.replicate (fromInteger $ natVal (Proxy @n))

-- | construct a record with values given by the constrained expression. Ex:
--
--  > new @Num @'[Int,Double,Word] @Sum (3 * 2) = Sum (6::Int) :& Sum (6.0::Double) :& Sum (6::Word) :& RNil
--
--  O(n)
new :: forall (c :: * -> Constraint) (xs :: [*]) f. (AllF c f xs, KnownNat (Length xs))
     => (forall x. c (f x) => f x) -> Rec f xs
new = new' @Type @c
-- | Create a recoord with values given by the constrained expression. Like @new@ but polykinded.
-- O(n)
new' :: forall (c :: * -> Constraint) (xs :: [k]) f. (AllF c f xs, KnownNat (Length xs))
     => (forall x. c (f x) => f x) -> Rec f xs
new' x = runST $ do
  xs <- M.new# @f @xs
  xs' <- M.crmap @c (\_ -> x) xs
  freeze# xs'
-- | Convert a vector into a homogeneous record of statically known size.
-- O(1)
fromVectorN :: forall n f x. KnownNat n => Vector (f x) -> Maybe (Rec f (Replicate n x))
fromVectorN v =
  let
    n = intVal @n
  in
    if n <= V.length v then Just . Rec# . mapCast# @Any $ V.take n v else Nothing
-- | Convert a vector into a homogeneous record with dynamically known size.
-- O(n)
fromVector :: forall f (x :: u). Vector (f x) -> Some (Rec f)
fromVector v = case someNatVal (fromIntegral $ V.length v) of
  Nothing -> error "fromVector: impossible! Negative vector length"
  Just (SomeNat (Proxy :: Proxy n))
    -> Some $ Rec# @u @f @(Replicate n x) $ mapCast# @Any v

-- | Transform a record by appling an endofunctor at the index.
-- O(n)
modify :: forall n f xs y. KnownNat n
       => (f (xs !! n) -> f y) -> Rec f xs -> Rec f (SetAt n xs y)
modify f r = runST $ do
  mr <- thaw r
  mr' <- M.modify @n f mr
  freeze# mr'

-- | Index into a statically known element of a record.
-- O(1)
index :: forall n f xs. KnownNat n => Rec f xs -> f (xs !! n)
index (Rec# v) = cast# $ v V.! intVal @n
-- | Prepare a dynamically known index into a statically known record.
-- O(n)
checkIndex :: forall (xs :: [u]) f. KnownNat (Length xs) => Rec f xs -> Int -> MaybeSome (Index xs)
checkIndex (Rec# (length -> n)) i | i < n = case someNatVal (fromIntegral i) of
  Just (SomeNat (Proxy :: Proxy n)) -> JustSome $ Index# @u @xs @(xs !! n) i
  Nothing -> error "Impossible! Negative Vector.length in checkIndex"
checkIndex _ _ = None

-- | Prepare a dynamically known index.
-- O(n)
checkIndex' :: forall (xs :: [u]) f. Rec f xs -> Int -> MaybeSome (Index xs)
checkIndex' RNil _ = None
checkIndex' ((_::f x) :& _) 0 = JustSome (RZ @u @x)
checkIndex' (_ :& xs) n = case checkIndex' xs (n-1) of
  None -> None
  JustSome i -> JustSome $ RS i
