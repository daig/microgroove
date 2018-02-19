{-# language MagicHash #-}
{-# language AllowAmbiguousTypes #-}
module Data.Microgroove.Index (RIndex(RIndex#,RZ,RS), mkIndex) where
import Data.Microgroove.Lib

-- | A prepared index into a record, allowing fast access
newtype RIndex (xs :: [u]) (x :: u) = RIndex# Int deriving Show
-- | An intermediate type for refining indexes
data RIndex' xs x where
  RZ' :: RIndex' (x ': xs) x
  RS' :: RIndex xs x -> RIndex' (y ': xs) x
-- | Refine an index type
upRIndex :: RIndex xs x -> RIndex' xs x
upRIndex (RIndex# i) = case i of
  0 -> cast# RZ'
  _ -> cast# $ RS' $ RIndex# $ i-1
-- | Construct or pattern match the zero index, refining its type
pattern RZ :: RIndex (x ': xs) (x :: u)
pattern RZ = RIndex# 0
-- | Construct or pattern match a successor index, refining its type
pattern RS :: RIndex (xs :: [u]) (x :: u) -> RIndex (y ': xs) x
pattern RS i <- (upRIndex -> RS' i) where
  RS (RIndex# i) = RIndex# (1+i)

  
-- | Construct a statically known index into a record
-- O(1)
mkIndex :: forall n xs . (KnownNat n, n <= Length xs - 1) => RIndex xs (xs !! n)
mkIndex = RIndex# $ intVal @n
