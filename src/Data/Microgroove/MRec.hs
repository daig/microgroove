{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language UndecidableSuperClasses #-}
{-# language UndecidableInstances #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove.MRec where
import Data.Microgroove.TypeLevel
import Data.Microgroove.Lib

import Data.Vector (Vector)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.Exts (Any,Constraint)
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad(..))

import GHC.TypeLits
import Data.Proxy


newtype MRec s (f :: u -> *) (us :: [u]) = MRec# (MVector s Any)
data MRec' s (f :: u -> *) (us :: [u]) where
  MRNil' :: MRec' s f '[]
  MRCons' :: MVector s (f u) -> MRec s f us -> MRec' s f (u ': us)

splitCons# :: MRec s f (x ': xs) -> (MVector s (f x),MRec s f xs)
splitCons# (MRec# v) = (cast# $ VM.take 1 v, MRec# $ VM.tail v)

pattern MRCons# :: MVector s (f x) -> MRec s f xs -> MRec s f (x ': xs)
pattern MRCons# x xs <- (( \ (MRec# v) -> (cast# $ VM.take 1 v, MRec# $ VM.tail v)) -> (x,xs)) -- where
  {-MRCons# x (MRec# xs) = MRec# (VM.cons (cast# x) xs)-}

upMRec :: MRec s f us -> MRec' s f us
upMRec (MRec# v) | VM.null v = cast# MRNil'
                 | otherwise = cast# $ MRCons' (cast# $ VM.take 1 v) (MRec# $ VM.tail v)

pattern MRNil :: () => (us ~ '[]) => MRec s f us
pattern MRNil <- (upMRec -> MRNil')

{-newtype Id a = Id a deriving Show-}
{-instance Functor Id where fmap f (Id a) = Id (f a)-}
{-{-pattern RCons :: () => (us' ~ (u ': us)) => f u -> Rec f us -> Rec f us'-}-}
pattern MRCons :: () => (us' ~ (u ': us)) => VM.MVector s (f u) -> MRec s f us -> MRec s f us'
pattern MRCons x xs <- (upMRec -> MRCons' x xs)


{-type family ((xs :: [u]) !! (n :: Nat)) where-}
  {-(x ': _) !! 0 = x-}
  {-(_ ': xs) !! n = xs !! (n-1)-}

{-newtype RIndex (xs :: [u]) (x :: u) = RIndex# Int deriving Show-}
{-data RIndex' xs x where-}
  {-RZ' :: RIndex' (x ': xs) x-}
  {-RS' :: RIndex xs x -> RIndex' (y ': xs) x-}
{-upRIndex :: RIndex xs x -> RIndex' xs x-}
{-upRIndex (RIndex# i) = case i of-}
  {-0 -> cast# RZ'-}
  {-_ -> cast# $ RS' $ RIndex# $ i-1-}
{-pattern RZ :: RIndex (x ': xs) (x :: u)-}
{-pattern RZ = RIndex# 0-}
{-pattern RS :: RIndex (xs :: [u]) (x :: u) -> RIndex (y ': xs) x-}
{-pattern RS i <- (upRIndex -> RS' i) where-}
  {-RS (RIndex# i) = RIndex# (1+i)-}

{-type family Length (xs :: [u]) :: Nat where-}
  {-Length '[] = 0-}
  {-Length (_ ': xs) = 1 + Length xs-}
  
{-mkIndex :: forall n xs . (KnownNat n, n <= Length xs - 1) => RIndex xs (xs !! n)-}
{-mkIndex = RIndex# $ fromInteger $ natVal (Proxy @n) -}

{-data Some f = forall x. Some (f x)-}
{-data MaybeSome f = forall x. JustSome (f x) | None-}

{-index :: forall n f xs. KnownNat n => Rec f xs -> f (xs !! n)-}
{-index (Rec# v) = cast# $ v V.! fromInteger (natVal (Proxy @n))-}
{-checkIndex :: forall (xs :: [u]) f. KnownNat (Length xs) => Rec f xs -> Int -> MaybeSome (RIndex xs)-}
{-checkIndex (Rec# (length -> n)) i | i < n = case someNatVal (fromIntegral i) of-}
  {-Just (SomeNat (Proxy :: Proxy n)) -> JustSome $ RIndex# @u @xs @(xs !! n) i-}
  

{-(!) :: Rec f us -> RIndex us u -> f u-}
{-Rec# v ! RIndex# i = cast# $ v V.! i-}

{-aa = RCons (Id (1::Integer)) $ RCons (Id (0.1::Double)) $ RCons (Id (3::Int)) RNil-}

{-class (c x,cc x) => (c :**: cc) x-}
{-instance (c x, cc x) => (c :**: cc) x-}

{-type family AllF (c :: * -> Constraint) (f :: u -> *) (xs :: [u]) :: Constraint where-}
  {-AllF c f '[] = ()-}
  {-AllF c f (x ': xs) = (Head (x ': xs) ~ x, Tail (x ': xs) ~ xs, c (f x),AllF c f xs)-}
{-type family All (c :: u -> Constraint) (xs :: [u]) :: Constraint where-}
  {-All c '[] = ()-}
  {-All c (x ': xs) = (c x,All c xs)-}

{-instance Show (Rec f '[]) where show RNil = "[]"-}
{-instance (Show (f x), Show (Rec f xs)) => Show (Rec f (x ': xs)) where-}
  {-show (RCons a xs) = show a ++ " : " ++ show xs-}

{-{-rmap :: (forall x. f x -> g x) -> Rec f xs -> Rec g xs-}-}
{-{-rmap f (Rec# v) = -}-}

{-cmap :: forall (c :: * -> Constraint) f g xs. All c xs-}
     {-=> (forall x. c x => f x -> g x) -> Rec f xs -> Rec g xs-}
{-cmap f = \case-}
  {-RNil -> RNil-}
  {-RCons x xs -> RCons (f x) (cmap @c f xs)-}
{-cfmap :: forall (c :: * -> Constraint) m f g xs. (AllF c f xs, PrimMonad m)-}
     {-=> (forall x. c (f x) => f x -> g x) -> MRec (PrimState m) f xs -> m (MRec (PrimState m) g xs)-}
{-cfmap f (MRec# v) = go @xs 0 >> pure (MRec# v) where-}
  {-go :: forall as. c (f (Head as)) => Int -> m ()-}
  {-go i = VM.modify v (cast# . f . cast# @(f (Head as))) i-}
cfmap :: forall (c :: * -> Constraint) g m f xs. (AllF c f xs, PrimMonad m)
     => (forall x. c (f x) => f x -> g x) -> MRec (PrimState m) f xs -> m (MRec (PrimState m) g xs)
cfmap f xs = cast# xs <$ go xs where
  go :: forall as. (AllF c f as) => MRec (PrimState m) f as -> m ()
  go = \case
    x@MRNil -> pure ()
    MRCons x xs -> VM.modify x (castf# @f . f) 0 >> go xs



type family Head (xs :: [u]) where Head (x ': _) = x
type family Tail (xs :: [u]) where Tail (_ ': xs) = xs
