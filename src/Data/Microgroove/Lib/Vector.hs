{-# language MagicHash #-}
module Data.Microgroove.Lib.Vector (subVector#) where

import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.Mutable (MVector)
import Data.Vector.Fusion.Stream.Monadic (Stream(..),Step(..))
import Data.Vector.Fusion.Bundle.Monadic (fromStream)
import Data.Vector.Fusion.Bundle.Size
import Control.Monad.Primitive

-- | Select elements in an ascending list of indicies
subStream# :: Applicative m => [Int] -> Stream m a -> Stream m a
{-# inline[1] subStream# #-}
subStream# ns (Stream step t) = Stream step' (ns,0,t)
  where
    {-# inline[0] step' #-}
    step' ([],_,_) = pure Done
    step' (n' : ns',n,s) = step s <&> \case
        Yield x s' -> if n==n' then Yield x (ns',n+1,s')
                               else Skip    (n':ns',n+1,s')
        Skip    s' -> Skip (n':ns,n,s')
        Done       -> Done

-- | Select elements in an ascending list of indicies. The @Int@ must be the length of the @[Int]@ list, and the @[Int]@ list must be in ascending order, but neither are checked.
subVector# :: PrimMonad m => Int -> [Int] -> MVector (PrimState m) x -> m (MVector (PrimState m) x)
subVector# nsLength# ns (VGM.mstream -> vm) = VGM.munstream $ fromStream (subStream# ns vm) (Exact nsLength#)

-- | @<$>@ with arguments reversed.
(<&>) :: Functor f => f a -> (a -> b) -> f b
{-# INLINE (<&>) #-}
a <&> f = f <$> a
