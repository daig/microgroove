{-# language MagicHash #-}
{-# language FlexibleContexts #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove.Index (
  -- * Prepared Indicies into a Record
  Index(Index#,RZ,RS)
  -- * Constructing Indicies
  ,mkIndex
  ,checkIndex, checkIndex') where
import Data.Microgroove.Lib
import GHC.TypeLits

-- | A prepared index into a record, allowing fast access
newtype Index (xs :: [u]) (x :: u) = Index# Int deriving Show
-- | An intermediate type for refining indexes
data Index' xs x where
  RZ' :: Index' (x ': xs) x
  RS' :: Index xs x -> Index' (y ': xs) x
-- | Refine an index type
upIndex :: Index xs x -> Index' xs x
upIndex (Index# i) = case i of
  0 -> cast# RZ'
  _ -> cast# $ RS' $ Index# $ i-1
-- | Construct or pattern match the zero index, refining its type
pattern RZ :: Index (x ': xs) (x :: u)
pattern RZ = Index# 0
-- | Construct or pattern match a successor index, refining its type
pattern RS :: Index (xs :: [u]) (x :: u) -> Index (y ': xs) x
pattern RS i <- (upIndex -> RS' i) where
  RS (Index# i) = Index# (1+i)

  
-- | Construct a statically known index into a record.
-- O(1)
mkIndex :: forall n xs . (KnownNat n, n <= Length xs - 1) => Index xs (xs !! n)
mkIndex = Index# $ intVal @n

-- | Prepare a dynamically known index into a statically known record.
-- O(n) and better constants than @checkIndex'@
checkIndex :: forall (xs :: [*]). KnownNat (Length xs) => Int -> MaybeSome (Index xs)
checkIndex = checkIndex' @Type @xs

-- | Prepare a dynamically known index into a statically known record. Like @checkIndex@ but polykinded
-- O(n) and better constants than @checkIndex'@
checkIndex' :: forall (xs :: [u]). KnownNat (Length xs) => Int -> MaybeSome (Index xs)
checkIndex' i | i < (intVal @(Length xs)) = case someNatVal (fromIntegral i) of
  Just (SomeNat (Proxy :: Proxy n)) -> JustSome $ Index# @u @xs @(xs !! n) i
  Nothing -> error "Impossible! Negative Vector.length in checkIndex"
             | otherwise = None
