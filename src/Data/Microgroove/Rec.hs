{-# language MagicHash #-}
{-# language FlexibleContexts #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove.Rec
  (Rec(Rec#,RNil,RCons), MRec(..)
  ,RIndex, mkIndex, index, (!), checkIndex, checkIndex'
  ,splitCons, rappend, rmap, crmap
  ,toVector, ctoVector
  ,fromVectorN, fromVector, replicate
  ,thaw, thaw#, freeze, freeze#
  ,modify
  ,module X
  ) where
import Prelude hiding (replicate)
import Data.Microgroove.MRec (MRec(..))
import qualified Data.Microgroove.MRec as M
import Data.Microgroove.Lib
import Data.Microgroove.TypeLevel as X

import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Exts (Any)
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad(..))
import GHC.TypeLits

import Data.Proxy

-- | Copy a record into a fresh mutable record
-- O(n)
thaw :: PrimMonad m => Rec f us -> m (MRec (PrimState m) f us)
thaw (Rec# v) = MRec# <$> V.thaw v
-- | unsafely thaw a record. The original record should no longer be used, but this is not checked
-- O(1)
thaw# :: PrimMonad m => Rec f us -> m (MRec (PrimState m) f us)
thaw# (Rec# v) = MRec# <$> V.unsafeThaw v
-- | Copy a mutable record into a fresh record
-- O(n)
freeze :: PrimMonad m => MRec (PrimState m) f us -> m (Rec f us)
freeze (MRec# v) = Rec# <$> V.freeze v
-- | Unsafely freeze a mutable record. The original record should no longer be modified, but this is not checked
-- O(1)
freeze# :: PrimMonad m => MRec (PrimState m) f us -> m (Rec f us)
freeze# (MRec# v) = Rec# <$> V.unsafeFreeze v

-- | A heterogeneous record represented by an untyped vector
newtype Rec (f :: u -> *) (us :: [u]) = Rec# (V.Vector Any)
-- | A dynamically shaped record, with elements satisfying some constraint
{-data SomeRec c f = forall us. AllF c f us => SomeRec (Rec f us)-}

-- | An intermediate type to deconstruct an @Rec@ into head normal form
data Rec' (f :: u -> *) (us :: [u]) where
  RNil' :: Rec' f '[]
  RCons' :: f u -> Rec f us -> Rec' f (u ': us)

-- | Split a record into a head element, and the remaining record
-- must be statically known to be nonempty
-- O(1)
splitCons :: Rec f (x ': xs) -> (f x,Rec f xs)
splitCons (Rec# v) = (cast# $ V.head v, Rec# $ V.tail v)

-- | Pattern match the head of a record that is statically known to be nonempty
-- Or prepend an element to a record
-- Matching is O(1), prepending is O(n)
{-pattern RCons# :: f x -> Rec f xs -> Rec f (x ': xs)-}
{-pattern RCons# x xs <- (( \ (Rec# v) -> (cast# $ V.head v, Rec# $ V.tail v)) -> (x,xs)) where-}
  {-RCons# x (Rec# xs) = Rec# (V.cons (cast# x) xs)-}

-- | Convert a Rec to head normal form,
-- refining the type to distinguish empty from nonempty records
upRec :: Rec f us -> Rec' f us
upRec (Rec# v) | V.null v = cast# RNil'
               | otherwise = cast# $ RCons' (cast# $ V.head v) (Rec# $ V.tail v)

-- | Construct or pattern match an empty record, refining its type
pattern RNil :: () => (us ~ '[]) => Rec f us
pattern RNil <- (upRec -> RNil') where
  RNil = Rec# V.empty

-- | Construct or pattern match a nonempty record, refining its type
-- Matching is O(1), prepending is O(n)
pattern RCons :: () => (us' ~ (u ': us)) => f u -> Rec f us -> Rec f us'
pattern RCons x xs <- (upRec -> RCons' x xs) where
  RCons x (Rec# xs) = Rec# (V.cons (cast# x) xs)

-- | A prepared index into a record, allowing fast access
newtype RIndex (xs :: [u]) (x :: u) = RIndex# Int deriving Show
-- | An intermediate type for refining indexes
data RIndex' xs x where
  RZ' :: RIndex' (x ': xs) x
  RS' :: RIndex xs x -> RIndex' (y ': xs) x
-- | Refine an index type
upRIndex :: RIndex xs x -> RIndex' xs x
upRIndex (RIndex# i) = case i of
  0 -> cast# RZ'
  _ -> cast# $ RS' $ RIndex# $ i-1
-- | Construct or pattern match the zero index, refining its type
pattern RZ :: RIndex (x ': xs) (x :: u)
pattern RZ = RIndex# 0
-- | Construct or pattern match a successor index, refining its type
pattern RS :: RIndex (xs :: [u]) (x :: u) -> RIndex (y ': xs) x
pattern RS i <- (upRIndex -> RS' i) where
  RS (RIndex# i) = RIndex# (1+i)

  
-- | Construct a statically known index into a record
-- O(1)
mkIndex :: forall n xs . (KnownNat n, n <= Length xs - 1) => RIndex xs (xs !! n)
mkIndex = RIndex# $ fromInteger $ natVal (Proxy @n) 


-- | Index into a statically known element of a record
-- O(1)
index :: forall n f xs. KnownNat n => Rec f xs -> f (xs !! n)
index (Rec# v) = cast# $ v V.! fromInteger (natVal (Proxy @n))
-- | Prepare a dynamically known index into a statically known record
-- O(n)
checkIndex :: forall (xs :: [u]) f. KnownNat (Length xs) => Rec f xs -> Int -> MaybeSome (RIndex xs)
checkIndex (Rec# (length -> n)) i | i < n = case someNatVal (fromIntegral i) of
  Just (SomeNat (Proxy :: Proxy n)) -> JustSome $ RIndex# @u @xs @(xs !! n) i
  Nothing -> error "Impossible! Negative Vector.length in checkIndex"
checkIndex _ _ = None

-- | Prepare a dynamically known index
-- O(n)
checkIndex' :: forall (xs :: [u]) f. Rec f xs -> Int -> MaybeSome (RIndex xs)
checkIndex' RNil _ = None
checkIndex' (RCons (_::f x) _) 0 = JustSome (RZ @u @x)
checkIndex' (RCons _ xs) n = case checkIndex' xs (n-1) of
  None -> None
  JustSome i -> JustSome $ RS i
checkIndex' _ _ = error "Impossible! RNil and RCons inexhaustive in checkIndex'"
  

-- | Index into a record with a prepared index
-- O(1)
(!) :: Rec f us -> RIndex us u -> f u
Rec# v ! RIndex# i = cast# $ v V.! i

instance Show (Rec f '[]) where
  show RNil = "[]"
  show _ = error "Impossible! RNil inexhaustive in show @(Rec f '[])"
instance (Show (f x), Show (Rec f xs)) => Show (Rec f (x ': xs)) where
  show (RCons a xs) = show a ++ " : " ++ show xs
  show _ = error "Impossible! RCons inexhaustive in show @(Rec f (x ': xs))"

-- | Append two records
-- O(n+m)
rappend :: Rec f as -> Rec f bs -> Rec f (as ++ bs)
rappend (Rec# as) (Rec# bs) = Rec# $ as V.++ bs

-- | Transform a mutable record in by mapping a natural tranformation.
-- O(n)
rmap :: (forall x. f x -> g x) -> Rec f xs -> Rec g xs
rmap f r = runST $ freeze# =<< M.rmap f =<< thaw r

-- | Transform a record in by mapping a natural tranformation that can make use of the provided constraint.
-- Ex: `crmap @Show (K . show) :: (Rec f xs) -> (MRec (K String) xs)`
-- O(n)
crmap :: forall c g f xs. AllF c f xs
      => (forall x. c (f x) => f x -> g x) -> Rec f xs -> Rec g xs
crmap f r = runST $ freeze# =<< M.crmap @c f =<< thaw r

-- | Convert a record to a vector by mapping to a homogeneous type
-- O(n)
toVector :: (forall x. f x -> r) -> Rec f xs -> Vector r
toVector f r = runST $ V.unsafeFreeze =<< M.toMVector f =<< thaw r

-- | Convert a record to a vector by mapping to a homogeneous type, making use of provided constraint
-- O(n)
ctoVector :: forall c r f xs. AllF c f xs
         => (forall x. c (f x) => f x -> r) -> Rec f xs -> Vector r
ctoVector f r = runST $ V.unsafeFreeze =<< M.ctoMVector @c f =<< thaw r

-- | Create a record of statically known size by replicating a single element
-- O(n)
replicate :: forall n f x. KnownNat n => f x -> Rec f (Replicate n x)
replicate = Rec# . mapCast# @Any . V.replicate (fromInteger $ natVal (Proxy @n))
-- | Convert a vector into a homogeneous record of statically known size 
-- O(1)
fromVectorN :: forall n f x. KnownNat n => Vector (f x) -> Maybe (Rec f (Replicate n x))
fromVectorN v =
  let
    n = fromInteger $ natVal (Proxy @n)
  in
    if n <= V.length v then Just . Rec# . mapCast# @Any $ V.take n v else Nothing
-- | Convert a vector into a homogeneous record with dynamically known size
-- O(n)
fromVector :: forall f (x :: u). Vector (f x) -> Some (Rec f)
fromVector v = case someNatVal (fromIntegral $ V.length v) of
  Nothing -> error "fromVector: impossible! Negative vector length"
  Just (SomeNat (Proxy :: Proxy n))
    -> Some $ Rec# @u @f @(Replicate n x) $ mapCast# @Any v

-- | Transform a record by appling an endofunctor at the index. O(n)
modify :: forall n f xs y. KnownNat n
       => (f (xs !! n) -> f y) -> Rec f xs -> Rec f (SetAt n xs y)
modify f r = runST $ do
  mr <- thaw r
  mr' <- M.modify @n f mr
  freeze# mr'
