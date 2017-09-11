{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableSuperClasses #-}
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

cast# :: forall b a. a -> b
cast# = unsafeCoerce

newtype Rec (f :: u -> *) (us :: [u]) = Rec# (V.Vector Any)
data Rec' (f :: u -> *) (us :: [u]) where
  RNil' :: Rec' f '[]
  RCons' :: f u -> Rec f us -> Rec' f (u ': us)

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

newtype RIndex x xs = RIndex# Int deriving Show
data RIndex' x xs where
  RZ' :: RIndex' x (x ': xs)
  RS' :: RIndex x xs -> RIndex' x (y ': xs)
upRIndex :: RIndex x xs -> RIndex' x xs
upRIndex (RIndex# i) = case i of
  0 -> cast# RZ'
  _ -> cast# $ RS' $ RIndex# $ i-1
pattern RZ :: RIndex (x :: u) (x ': xs)
pattern RZ = RIndex# 0
pattern RS :: RIndex (x :: u) (xs :: [u]) -> RIndex x (y ': xs)
pattern RS i <- (upRIndex -> RS' i) where
  RS (RIndex# i) = RIndex# (1+i)
{-checkIndex :: Rec f (xs :: [u]) -> Int -> Maybe (RIndex (x :: u) (xs :: [u]))-}
{-checkIndex (RCons (_ :: f u) _)  0 = Just (RZ :: RIndex u xs)-}
{-checkIndex n = if n > 0 then RS <$> checkIndex (n-1) else Nothing-}

(!) :: Rec f us -> RIndex u us -> f u
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
