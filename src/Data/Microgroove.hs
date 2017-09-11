{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language UndecidableSuperClasses #-}
{-# language UndecidableInstances #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove where
import Data.Vector (Vector)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts (Any,Constraint)
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad(..))

import GHC.TypeLits
import Data.Proxy

cast# :: forall b a. a -> b
cast# = unsafeCoerce

newtype Rec (f :: u -> *) (us :: [u]) = Rec# (V.Vector Any)
data Rec' (f :: u -> *) (us :: [u]) where
  RNil' :: Rec' f '[]
  RCons' :: f u -> Rec f us -> Rec' f (u ': us)

splitCons :: Rec f (x ': xs) -> (f x,Rec f xs)
splitCons (Rec# v) = (cast# $ V.head v, Rec# $ V.tail v)

pattern RCons# :: f x -> Rec f xs -> Rec f (x ': xs)
pattern RCons# x xs <- (( \ (Rec# v) -> (cast# $ V.head v, Rec# $ V.tail v)) -> (x,xs)) where
  RCons# x (Rec# xs) = Rec# (V.cons (cast# x) xs)

upRec :: Rec f us -> Rec' f us
upRec (Rec# v) | V.null v = cast# RNil'
               | otherwise = cast# $ RCons' (cast# $ V.head v) (Rec# $ V.tail v)

pattern RNil :: () => (us ~ '[]) => Rec f us
pattern RNil <- (upRec -> RNil') where
  RNil = Rec# V.empty

newtype Id a = Id a deriving Show
instance Functor Id where fmap f (Id a) = Id (f a)
{-pattern RCons :: () => (us' ~ (u ': us)) => f u -> Rec f us -> Rec f us'-}
pattern RCons :: () => (us' ~ (u ': us)) => f u -> Rec f us -> Rec f us'
pattern RCons x xs <- (upRec -> RCons' x xs) where
  RCons x (Rec# xs) = Rec# (V.cons (cast# x) xs)


type family ((xs :: [u]) !! (n :: Nat)) where
  (x ': _) !! 0 = x
  (_ ': xs) !! n = xs !! (n-1)

newtype RIndex (xs :: [u]) (x :: u) = RIndex# Int deriving Show
data RIndex' xs x where
  RZ' :: RIndex' (x ': xs) x
  RS' :: RIndex xs x -> RIndex' (y ': xs) x
upRIndex :: RIndex xs x -> RIndex' xs x
upRIndex (RIndex# i) = case i of
  0 -> cast# RZ'
  _ -> cast# $ RS' $ RIndex# $ i-1
pattern RZ :: RIndex (x ': xs) (x :: u)
pattern RZ = RIndex# 0
pattern RS :: RIndex (xs :: [u]) (x :: u) -> RIndex (y ': xs) x
pattern RS i <- (upRIndex -> RS' i) where
  RS (RIndex# i) = RIndex# (1+i)

type family Length (xs :: [u]) :: Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs
  
mkIndex :: forall n xs . (KnownNat n, n <= Length xs - 1) => RIndex xs (xs !! n)
mkIndex = RIndex# $ fromInteger $ natVal (Proxy @n) 

data Some f = forall x. Some (f x)
data MaybeSome f = forall x. JustSome (f x) | None

index :: forall n f xs. KnownNat n => Rec f xs -> f (xs !! n)
index (Rec# v) = cast# $ v V.! fromInteger (natVal (Proxy @n))
checkIndex :: forall (xs :: [u]) f. KnownNat (Length xs) => Rec f xs -> Int -> MaybeSome (RIndex xs)
checkIndex (Rec# (length -> n)) i | i < n = case someNatVal (fromIntegral i) of
  Just (SomeNat (Proxy :: Proxy n)) -> JustSome $ RIndex# @u @xs @(xs !! n) i
  

(!) :: Rec f us -> RIndex us u -> f u
Rec# v ! RIndex# i = cast# $ v V.! i

aa = RCons (Id (1::Integer)) $ RCons (Id (0.1::Double)) $ RCons (Id (3::Int)) RNil

class (c x,cc x) => (c :**: cc) x
instance (c x, cc x) => (c :**: cc) x

type family AllF (c :: * -> Constraint) (f :: u -> *) (xs :: [u]) :: Constraint where
  AllF c f '[] = ()
  AllF c f (x ': xs) = (c (f x),AllF c f xs)
type family All (c :: u -> Constraint) (xs :: [u]) :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x,All c xs)

instance Show (Rec f '[]) where show RNil = "[]"
instance (Show (f x), Show (Rec f xs)) => Show (Rec f (x ': xs)) where
  show (RCons a xs) = show a ++ " : " ++ show xs

{-rmap :: (forall x. f x -> g x) -> Rec f xs -> Rec g xs-}
{-rmap f (Rec# v) = -}

cmap :: forall (c :: * -> Constraint) f g xs. All c xs
     => (forall x. c x => f x -> g x) -> Rec f xs -> Rec g xs
cmap f = \case
  RNil -> RNil
  RCons x xs -> RCons (f x) (cmap @c f xs)
cfmap :: forall (c :: * -> Constraint) f g xs. AllF c f xs
     => (forall x. c (f x) => f x -> g x) -> Rec f xs -> Rec g xs
cfmap f = \case
  RNil -> RNil
  RCons x xs -> RCons (f x) (cfmap @c f xs)
