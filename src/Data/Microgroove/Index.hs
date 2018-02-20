{-# language MagicHash #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove.Index (Index(Index#,RZ,RS), mkIndex) where
import Data.Microgroove.Lib

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
