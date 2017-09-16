{-# language UndecidableInstances #-}
{-# language TypeFamilyDependencies #-}
{-# language AllowAmbiguousTypes #-}
{-# language UndecidableSuperClasses #-}
{-# language MultiParamTypeClasses #-}
module Data.Microgroove.TypeLevel where
import GHC.Exts (Constraint)
import GHC.TypeLits
import Data.Proxy


type family ((xs :: [u]) !! (n :: Nat)) where
  (x ': _) !! 0 = x
  (_ ': xs) !! n = xs !! (n-1)

type family Length (xs :: [u]) :: Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs

type family AllF (c :: * -> Constraint) (f :: u -> *) (xs :: [u]) :: Constraint where
  AllF c f '[] = ()
  AllF c f (x ': xs) = (c (f x),AllF c f xs)
class AllF c f xs => AllF' c f xs 
instance AllF c f xs => AllF' c f xs 

type family (as :: [k]) ++ (bs :: [k]) where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

type family Head (xs :: [u]) where Head (x ': _) = x
type family Tail (xs :: [u]) where Tail (_ ': xs) = xs

data Nat' = Z | S Nat'
type family Replicate' (n :: Nat') (x :: u) = xs | xs -> n where
  Replicate' Z _ = '[]
  Replicate' (S n) x = x ': Replicate' n x
  
type family Replicate (n :: Nat) (x :: u) :: [u] where
  Replicate 0 x = '[]
  Replicate n x = x ': Replicate (n-1) x
type family ReplicateC n (c :: Constraint) :: Constraint where
  ReplicateC 0 _ = ()
  ReplicateC n c = (c,ReplicateC (n-1) c)

data Some f = forall x. Some (f x)
data MaybeSome f = forall x. JustSome (f x) | None
