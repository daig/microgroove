{-# language MagicHash #-}
module Data.Microgroove.Mutable.Type (MRec(MRec#,MRNil,MRCons), module X) where

import Data.Microgroove.Lib
import Data.Microgroove.Lib as X (Any)
import Data.Vector.Mutable as X (MVector)
import qualified Data.Vector.Mutable as VM

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
