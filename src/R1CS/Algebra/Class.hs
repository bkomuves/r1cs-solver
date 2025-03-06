
module R1CS.Algebra.Class where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word
import Data.Proxy

--------------------------------------------------------------------------------

-- NOTE: ordering is required for technical reasons
class (Eq a, Ord a, Show a, Num a, Fractional a) => Field a where
  fieldName  :: Proxy a -> String
  fieldPrime :: Proxy a -> Integer
  showF      :: a -> String
  pow        :: a -> Int -> a
  power      :: a -> Integer -> a
  isZero     :: a -> Bool
  fieldSqrt1 :: a -> Maybe a
  toF        :: Integer -> a
  fromF      :: a -> Integer

class Field a => SmallField a where
  toF_   :: Int64 -> a
  fromF_ :: a -> Word64

--------------------------------------------------------------------------------

data Sqrt a 
  = NoRoot
  | DblRoot  !a
  | TwoRoots !a !a 
  deriving (Eq,Show)

sqrtToList :: Sqrt a -> [a]
sqrtToList  NoRoot        = []
sqrtToList (DblRoot  x  ) = [x]
sqrtToList (TwoRoots x y) = [x,y]

fieldSqrt :: Field a => a -> Sqrt a
fieldSqrt x = case fieldSqrt1 x of
  Nothing -> NoRoot
  Just y  -> if y == 0 
    then DblRoot  y 
    else TwoRoots y (-y)

--------------------------------------------------------------------------------