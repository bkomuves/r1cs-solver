
-- | The prime field with prime @p = 2^20 + 7 = 1048583@

 module R1CS.Algebra.TestField20 where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word
import Data.Bits
import Data.Ratio

import Data.Array

import R1CS.Algebra.SmallField
import qualified R1CS.Algebra.SmallField as Raw
import R1CS.Algebra.Class

--------------------------------------------------------------------------------

instance Field F20 where
  fieldName  _ = "F20"
  fieldPrime _ = fromIntegral fieldPrimeF20
  showF      = show . unF20
  pow        = powF20_
  power      = powF20
  isZero     = isZeroF20
  fieldSqrt1 = fieldSqrtF20
  toF        = \x -> F20 (fromInteger $ mod x 1048583)
  fromF      = fromIntegral . unF20

instance SmallField F20 where
  toF_   = toF20
  fromF_ = unF20

{-# SPECIALISE fieldSqrt :: F20 -> Sqrt F20 #-}

--------------------------------------------------------------------------------

fieldPrimeF20 :: Int
fieldPrimeF20 = 1048583

sqrtTable :: Array Word64 (Maybe F20)
sqrtTable = accumArray (flip const) Nothing (0,fromIntegral fieldPrimeF20 - 1) 
  [ (unF20 (x*x), Just x) | k<-[0..fieldPrimeF20-1], let x = F20 (fromIntegral k) ]

fieldSqrtF20 :: F20 -> Maybe F20
fieldSqrtF20 (F20 k) = sqrtTable ! k

--------------------------------------------------------------------------------

-- | prime field with @p = 2^20 + 7 = 1048583@
newtype F20 = F20 Word64 deriving (Eq,Ord,Show)

unF20 :: F20 -> Word64
unF20 (F20 x) = x

toF20 :: Int64 -> F20
toF20 = F20 . modF20

-- WARNING: fromInteger breaks if this is Word64, because it first converts
-- a negative integer to Word64, then takes modulo........ 
modF20 :: Int64 -> Word64
modF20 x = fromIntegral (mod x 1048583)

instance Num F20 where
  fromInteger = toF20 . fromInteger
  negate (F20 x)         = F20 $ Raw.neg 1048583 x
  (+)    (F20 x) (F20 y) = F20 $ Raw.add 1048583 x y
  (-)    (F20 x) (F20 y) = F20 $ Raw.sub 1048583 x y
  (*)    (F20 x) (F20 y) = F20 $ Raw.mul 1048583 x y  
  abs    x = x
  signum _ = toF20 1  

instance Fractional F20 where
  recip (F20 x)         = F20 $ Raw.inv 1048583 x
  (/)   (F20 x) (F20 y) = F20 $ Raw.div 1048583 x y
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)  

isZeroF20 :: F20 -> Bool
isZeroF20 (F20 x) = (x == 0)

powF20 :: F20 -> Integer -> F20
powF20 (F20 x) n = F20 $ Raw.pow' 1048583 x n

powF20_ :: F20 -> Int -> F20
powF20_ (F20 x) n = F20 $ Raw.pow 1048583 x (fromIntegral n)

-------------------------------------------------------------------------------
