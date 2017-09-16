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

upMRec :: MRec s f us -> MRec' s f us
upMRec (MRec# v) | VM.null v = cast# MRNil'
                 | otherwise = cast# $ MRCons' (cast# $ VM.take 1 v) (MRec# $ VM.tail v)

pattern MRNil :: () => (us ~ '[]) => MRec s f us
pattern MRNil <- (upMRec -> MRNil')

pattern MRCons :: () => (us' ~ (u ': us)) => VM.MVector s (f u) -> MRec s f us -> MRec s f us'
pattern MRCons x xs <- (upMRec -> MRCons' x xs)


rmap :: forall g m f xs. PrimMonad m => (forall x. f x -> g x) -> MRec (PrimState m) f xs -> m (MRec (PrimState m) g xs)
rmap f xs = cast# xs <$ go xs where
  go :: MRec (PrimState m) f as -> m ()
  go = \case
    x@MRNil -> pure ()
    MRCons x xs -> VM.modify x (castf# @f . f) 0 >> go xs

crmap :: forall (c :: * -> Constraint) g m f xs. (AllF c f xs, PrimMonad m)
     => (forall x. c (f x) => f x -> g x) -> MRec (PrimState m) f xs -> m (MRec (PrimState m) g xs)
crmap f xs = cast# xs <$ go xs where
  go :: forall as. (AllF c f as) => MRec (PrimState m) f as -> m ()
  go = \case
    x@MRNil -> pure ()
    MRCons x xs -> VM.modify x (castf# @f . f) 0 >> go xs

toMVector :: forall r m f xs. PrimMonad m
         => (forall x. f x -> r) -> MRec (PrimState m) f xs -> m (MVector (PrimState m) r)
toMVector f xs = cast# xs <$ go xs where
  go :: MRec (PrimState m) f as -> m ()
  go = \case
    x@MRNil -> pure ()
    MRCons x xs -> VM.modify x (cast# . f) 0 >> go xs

ctoMVector :: forall (c :: * -> Constraint) r m f xs. (AllF c f xs, PrimMonad m)
          => (forall x. c (f x) => f x -> r) -> MRec (PrimState m) f xs -> m (MVector (PrimState m) r)
ctoMVector f xs = cast# xs <$ go xs where
  go :: forall as. (AllF c f as) => MRec (PrimState m) f as -> m ()
  go = \case
    x@MRNil -> pure ()
    MRCons x xs -> VM.modify x (cast# . f) 0 >> go xs
