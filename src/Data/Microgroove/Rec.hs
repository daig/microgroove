{-# language MagicHash #-}
{-# language FlexibleContexts #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove.Rec where
import qualified Data.Microgroove.MRec as M
import Data.Microgroove.Lib
import Data.Microgroove.TypeLevel

import Data.Vector (Vector)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.Exts (Any)
import Control.Monad ((<=<))
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad(..))
import GHC.TypeLits

import Data.Proxy

thaw :: PrimMonad m => Rec f us -> m (M.MRec (PrimState m) f us)
thaw (Rec# v) = M.MRec# <$> V.thaw v
freeze :: PrimMonad m => M.MRec (PrimState m) f us -> m (Rec f us)
freeze (M.MRec# v) = Rec# <$> V.freeze v

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

{-pattern RCons :: () => (us' ~ (u ': us)) => f u -> Rec f us -> Rec f us'-}
pattern RCons :: () => (us' ~ (u ': us)) => f u -> Rec f us -> Rec f us'
pattern RCons x xs <- (upRec -> RCons' x xs) where
  RCons x (Rec# xs) = Rec# (V.cons (cast# x) xs)

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

  
mkIndex :: forall n xs . (KnownNat n, n <= Length xs - 1) => RIndex xs (xs !! n)
mkIndex = RIndex# $ fromInteger $ natVal (Proxy @n) 


index :: forall n f xs. KnownNat n => Rec f xs -> f (xs !! n)
index (Rec# v) = cast# $ v V.! fromInteger (natVal (Proxy @n))
checkIndex :: forall (xs :: [u]) f. KnownNat (Length xs) => Rec f xs -> Int -> MaybeSome (RIndex xs)
checkIndex (Rec# (length -> n)) i | i < n = case someNatVal (fromIntegral i) of
  Just (SomeNat (Proxy :: Proxy n)) -> JustSome $ RIndex# @u @xs @(xs !! n) i
  

(!) :: Rec f us -> RIndex us u -> f u
Rec# v ! RIndex# i = cast# $ v V.! i

aa :: Rec Id '[Integer, Double, Int]
aa = RCons (Id (1::Integer)) $ RCons (Id (0.1::Double)) $ RCons (Id (3::Int)) RNil
bb = do
  a <- thaw @IO aa
  b <- M.cfmap @Show @(K String) (K . show) a
  freeze b



instance Show (Rec f '[]) where show RNil = "[]"
instance (Show (f x), Show (Rec f xs)) => Show (Rec f (x ': xs)) where
  show (RCons a xs) = show a ++ " : " ++ show xs

{-rmap :: (forall x. f x -> g x) -> Rec f xs -> Rec g xs-}
{-rmap f (Rec# v) = -}

{-cmap :: forall (c :: * -> Constraint) f g xs. All c xs-}
     {-=> (forall x. c x => f x -> g x) -> Rec f xs -> Rec g xs-}
{-cmap f = \case-}
  {-RNil -> RNil-}
  {-RCons x xs -> RCons (f x) (cmap @c f xs)-}
{-cfmap :: forall (c :: * -> Constraint) f g xs. AllF c f xs-}
     {-=> (forall x. c (f x) => f x -> g x) -> Rec f xs -> Rec g xs-}
{-cfmap f = freeze . M.cfmap @c f <=< thaw-}


