{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove.Lib.TypeLevel (module Data.Microgroove.Lib.TypeLevel, module X) where
import GHC.TypeLits as X (Nat, type (-), type (+), type (<=), KnownNat)
import GHC.TypeLits (natVal)
import GHC.Exts as X (Constraint,Any)
import Data.Proxy as X (Proxy(..))
import Data.Kind as X (Type)

-- | Index into a type level list
type family ((xs :: [u]) !! (n :: Nat)) where
  (x ': _) !! 0 = x
  (_ ': xs) !! n = xs !! (n-1)

type family Drop (n :: Nat) (xs :: [u]) where
  Drop 0 xs = xs
  Drop n (x ': xs) = Drop (n-1) xs

class KnownNats ns where intList :: [Int]
instance KnownNats '[] where intList = []
instance (KnownNat n,KnownNats ns) => KnownNats (n ': ns) where intList = intVal @n : intList @ns

type family SubList (ns :: [Nat]) (xs :: [u]) :: [u] where
  SubList ns xs = SubList' ns xs 0

type family SubList' (ns :: [Nat]) (xs :: [u]) (acc :: Nat) :: [u] where
  SubList' '[] xs n = '[]
  SubList' (n ': ns) (x ': xs) n = x ': SubList' ns xs (n+1)
  SubList' ns (x ': xs) n = SubList' ns xs (n+1)

type family Length (xs :: [u]) :: Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs

type family All (c :: u -> Constraint) (xs :: [u]) :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)
-- | @AllF c f xs@ ensures that constraint @c (f x)@ holds for all @x@ in @xs@
type family AllF (c :: * -> Constraint) (f :: u -> *) (xs :: [u]) :: Constraint where
  AllF c f '[] = ()
  AllF c f (x ': xs) = (c (f x),AllF c f xs)

-- | Append type level lists
type family (as :: [k]) ++ (bs :: [k]) where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

-- | Extend a type @x@ into a type level list @xs@ of length @n@
type family Replicate (n :: Nat) (x :: u) :: [u] where
  Replicate 0 x = '[]
  Replicate n x = x ': Replicate (n-1) x

-- | Inductive Natural Numbers
{-data Nat' = Z | S Nat'-}
-- | An injective form of @Replicate@ using inductive @Nat'@ rather than builtins
{-type family Replicate' (n :: Nat') (x :: u) = xs | xs -> n where-}
  {-Replicate' 'Z _ = '[]-}
  {-Replicate' ('S n) x = x ': Replicate' n x-}
  
type family SetAt n xs x where
  SetAt 0 (_ ': xs) x = x ': xs
  SetAt n (x ': xs) y = x ': SetAt (n-1) xs y


intVal :: forall n. KnownNat n => Int
intVal = fromInteger (natVal (Proxy @n))
