{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# language MagicHash #-}
{-# language UndecidableInstances #-}
{-# language FlexibleContexts #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove (
  -- * Immutable Heterogeneous Records
   Rec(Rec#,RNil,(:&))
  -- ** Mutable / Immutable Conversions
  ,thaw, thaw#, freeze, freeze#
  -- ** Constructing Records
  , new, new'
  ,fromVectorN, fromVector, replicate
  -- ** Modifying Records
  ,modify
  ,rmap, rmapM
  -- *** Modification With Constraint
  ,crmap, crmapM
  -- ** Combining Records
  ,rappend
  ,rzip
  -- *** Combining with Constraint
  ,crzip
  -- ** Deconstructing Records
  ,splitCons
  ,foldMapF 
  ,toVector
  -- *** Deconstructing with Constraint
  ,cfoldMapF
  ,ctoVector
  -- ** Filtering Records
  ,subRecord#
  -- * Prepared Indicies into a Record
  ,Index(Index#,RZ,RS)
  -- ** Constructing Indicies
  ,mkIndex, checkIndex, checkDynIndex
  -- ** Indexing
  ,index, (!)
  ,module X
  ) where
import Prelude hiding (replicate)
import Data.Microgroove.Lib
import Data.Microgroove.Lib.TypeLevel as X
import Data.Microgroove.Index
import Data.Microgroove.Mutable (MRec(..))
import qualified Data.Microgroove.Mutable as M

import qualified Data.Vector as V
import Data.Vector as X (Vector)
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad(..))
import GHC.TypeLits
import GHC.Float
import GHC.Exts (IsList(..))
import Data.Monoid (Endo(..))

-- | A heterogeneous record represented by an untyped vector
newtype Rec (f :: u -> *) (us :: [u]) = Rec# (V.Vector Any)

-- A dynamically shaped record, with elements satisfying some constraint
{-data SomeRec c f = forall us. AllF c f us => SomeRec (Rec f us)-}

instance Show (Rec f '[]) where
  show RNil = "[]"
  show _ = error "Impossible! RNil inexhaustive in show @(Rec f '[])"
instance (Show (f x), Show (Rec f xs)) => Show (Rec f (x ': xs)) where
  show (a :& xs) = show a ++ " :& " ++ show xs
  show _ = error "Impossible! RCons inexhaustive in show @(Rec f (x ': xs))"

instance Eq (Rec f '[]) where RNil == RNil = True
instance Ord (Rec f '[]) where compare RNil RNil = EQ

-- | An intermediate type to deconstruct an @Rec@ into head normal form
data Rec' (f :: u -> *) (us :: [u]) where
  RNil' :: Rec' f '[]
  RCons' :: f u -> Rec f us -> Rec' f (u ': us)


-- | Convert a Rec to head normal form,
-- refining the type to distinguish empty from nonempty records
upRec :: Rec f us -> Rec' f us
upRec (Rec# v) | V.null v = cast# RNil'
               | otherwise = cast# $ RCons' (cast# $ V.head v) (Rec# $ V.tail v)

-- | Construct or pattern match an empty record, refining its type
pattern RNil :: () => (us ~ '[]) => Rec f us
pattern RNil <- (upRec -> RNil') where
  RNil = Rec# V.empty

-- | Construct or pattern match a nonempty record, refining its type.
-- Matching is O(1), prepending is O(n)
pattern (:&) :: () => (us' ~ (u ': us)) => f u -> Rec f us -> Rec f us'
pattern (:&) x xs <- (upRec -> RCons' x xs) where
  x :& (Rec# xs) = Rec# (V.cons (cast# x) xs)
{-# complete RNil, (:&) #-}
infixr 5 :&

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

instance Monoid (Rec f '[]) where
  mempty = RNil
  mappend _ _ = RNil
-- | @mappend@ fields pairwise
instance (KnownNat (Length (x ': xs)), AllF Monoid f ((x ': xs) :: [k])) => Monoid (Rec f (x ': xs)) where
  mempty = new' @k @Monoid @(x ': xs) mempty
  mappend = crzip' @k @Monoid mappend

instance Num (Rec f '[]) where
  fromInteger _ = RNil
  _ + _ = RNil
  _ - _ = RNil
  _ * _ = RNil
  negate x = x
  abs x = x
  signum x = x
instance (KnownNat (Length (x ': xs)), AllF Num f ((x ': xs) :: [k])) => Num (Rec f (x ': xs)) where
  fromInteger n = new' @k @Num (fromInteger n)
  (+) = crzip' @k @Num (+)
  (-) = crzip' @k @Num (-)
  (*) = crzip' @k @Num (*)
  negate = crmap @Num negate
  abs = crmap @Num abs
  signum = crmap @Num signum

instance Fractional (Rec f '[]) where
  fromRational _ = RNil
  _ / _ = RNil
  recip x = x
instance (KnownNat (Length (x ': xs)), AllF Num f (x ': xs), AllF Fractional f ((x ': xs) :: [k])) => Fractional (Rec f (x ': xs)) where
  fromRational n = new' @k @Fractional (fromRational n)
  (/) = crzip' @k @Fractional (/)
  recip = crmap @Fractional recip

instance Floating (Rec f '[]) where
  pi = RNil
  exp x = x
  log x = x
  sqrt x = x
  sin x = x
  cos x = x
  tan x = x
  asin x = x
  acos x = x
  atan x = x
  sinh x = x
  cosh x = x
  tanh x = x
  asinh x = x
  acosh x = x
  atanh x = x
  log1p x = x
  expm1 x = x
  log1pexp x = x
  log1mexp x = x

instance (KnownNat (Length (x ': xs)), Fractional (Rec f ((x ': xs) :: [k])), AllF Floating f (x ': xs)) => Floating (Rec f (x ': xs)) where
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

instance IsList (Rec f '[]) where
  type Item (Rec f '[]) = Any
  fromList = \case
    [] -> RNil
    _ -> error "fromList: nonempty list literal"
  fromListN 0 [] = RNil
  fromListN _ _ = error "fromListN: nonempty list literal"
  toList _ = []
instance KnownNat (Length (x ': xs)) => IsList (Rec f (x ': xs)) where
  type Item (Rec f (x ': xs)) = Any
  fromListN n xs =
    let n' = intVal @(Length (x ': xs))
    in if n == n'
     then Rec# $ fromListN n xs
     else error $ "fromListN: expected length " ++ show n' ++ " but actually " ++ show n
  fromList (fromList -> xs) =
    let n' = intVal @(Length (x ': xs))
        n = V.length xs
    in if n == n'
     then Rec# xs
     else error $ "fromListN: expected length " ++ show n' ++ " but actually " ++ show n
  toList = (`appEndo` []) . foldMapF (\x -> Endo (\xs -> cast# @Any x : xs))



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

-- | Prepare a dynamically known index.
-- O(n)
checkDynIndex :: forall (xs :: [u]) f. Rec f xs -> Int -> MaybeSome (Index xs)
checkDynIndex RNil _ = None
checkDynIndex ((_::f x) :& _) 0 = JustSome (RZ @u @x)
checkDynIndex (_ :& xs) n = case checkDynIndex xs (n-1) of
  None -> None
  JustSome i -> JustSome $ RS i

-- | Choose a satically known ordered subset of the fields in a record. Ex:
--
--  > subRecord @'[0,2] ([1] :& "a" :& ["wow","what"] :& [[1,2,3], [4,5]] :& RNil)
--  > = [1] :& ["wow","what"] :& RNil
--
--  @ns@ must be in ascending order, but this is not checked.
--  O(m)
subRecord# :: forall ns f xs. (KnownNat (Length ns), KnownNats ns)
          => Rec f xs -> Rec f (SubList# ns xs)
subRecord# (Rec# v) = Rec# $ v `V.backpermute` fromListN n ns
  where ns = intList @ns
        n = intVal @(Length ns)
