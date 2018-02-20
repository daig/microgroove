{-# language MagicHash #-}
{-# language FlexibleContexts #-}
module Data.Microgroove.Type
  (Rec(Rec#,RNil,(:&)), Rec'(..), upRec, module X) where
import qualified Data.Vector as V
import Data.Vector as X (Vector)
import Data.Microgroove.Lib as X (Any)
import Data.Microgroove.Lib

-- | A heterogeneous record represented by an untyped vector
newtype Rec (f :: u -> *) (us :: [u]) = Rec# (V.Vector Any)

-- A dynamically shaped record, with elements satisfying some constraint
{-data SomeRec c f = forall us. AllF c f us => SomeRec (Rec f us)-}

instance Show (Rec f '[]) where
  show RNil = "[]"
  show _ = error "Impossible! RNil inexhaustive in show @(Rec f '[])"
instance (Show (f x), Show (Rec f xs)) => Show (Rec f (x ': xs)) where
  show (a :& xs) = show a ++ " : " ++ show xs
  show _ = error "Impossible! RCons inexhaustive in show @(Rec f (x ': xs))"

instance Eq (Rec f '[]) where RNil == RNil = True
instance Ord (Rec f '[]) where compare RNil RNil = EQ

-- | An intermediate type to deconstruct an @Rec@ into head normal form
data Rec' (f :: u -> *) (us :: [u]) where
  RNil' :: Rec' f '[]
  RCons' :: f u -> Rec f us -> Rec' f (u ': us)


-- Pattern match the head of a record that is statically known to be nonempty
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

-- | Construct or pattern match a nonempty record, refining its type.
-- Matching is O(1), prepending is O(n)
pattern (:&) :: () => (us' ~ (u ': us)) => f u -> Rec f us -> Rec f us'
pattern (:&) x xs <- (upRec -> RCons' x xs) where
  x :& (Rec# xs) = Rec# (V.cons (cast# x) xs)
{-# complete RNil, (:&) #-}
infixr 5 :&
