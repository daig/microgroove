{-# language UndecidableInstances #-}
module Data.Microgroove.TypeLevel where
import GHC.Exts (Constraint)
import GHC.TypeLits


type family ((xs :: [u]) !! (n :: Nat)) where
  (x ': _) !! 0 = x
  (_ ': xs) !! n = xs !! (n-1)

type family Length (xs :: [u]) :: Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs

type family AllF (c :: * -> Constraint) (f :: u -> *) (xs :: [u]) :: Constraint where
  AllF c f '[] = ()
  AllF c f (x ': xs) = (c (f x),AllF c f xs)
type family All (c :: u -> Constraint) (xs :: [u]) :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x,All c xs)

{-class (c x,cc x) => (c :**: cc) x-}
{-instance (c x, cc x) => (c :**: cc) x-}
