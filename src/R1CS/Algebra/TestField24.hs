
-- | The prime field with prime @p = 2^24 + 43 = 16777259@

 module R1CS.Algebra.TestField24 where

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

instance Field F24 where
  fieldName  _ = "F24"
  fieldPrime _ = fromIntegral fieldPrimeF24
  showF      = show . unF24
  pow        = powF24_
  power      = powF24
  isZero     = isZeroF24
  fieldSqrt1 = fieldSqrtF24
  toF        = \x -> F24 (fromInteger $ mod x 16777259)
  fromF      = fromIntegral . unF24

instance SmallField F24 where
  toF_   = toF24
  fromF_ = unF24

{-# SPECIALISE fieldSqrt :: F24 -> Sqrt F24 #-}

--------------------------------------------------------------------------------

fieldPrimeF24 :: Int
fieldPrimeF24 = 16777259

sqrtTable :: Array Word64 (Maybe F24)
sqrtTable = accumArray (flip const) Nothing (0,fromIntegral fieldPrimeF24 - 1) 
  [ (unF24 (x*x), Just x) | k<-[0..fieldPrimeF24-1], let x = F24 (fromIntegral k) ]

fieldSqrtF24 :: F24 -> Maybe F24
fieldSqrtF24 (F24 k) = sqrtTable ! k

--------------------------------------------------------------------------------

-- | prime field with @p = 16777259 = 2^24 + 43@
newtype F24 = F24 Word64 deriving (Eq,Ord,Show)

unF24 :: F24 -> Word64
unF24 (F24 x) = x

toF24 :: Int64 -> F24
toF24 = F24 . modF24

-- WARNING: fromInteger breaks if this is Word64, because it first converts
-- a negative integer to Word64, then takes modulo........ 
modF24 :: Int64 -> Word64
modF24 x = fromIntegral (mod x 16777259)

instance Num F24 where
  fromInteger = toF24 . fromInteger
  negate (F24 x)         = F24 $ Raw.neg 16777259 x
  (+)    (F24 x) (F24 y) = F24 $ Raw.add 16777259 x y
  (-)    (F24 x) (F24 y) = F24 $ Raw.sub 16777259 x y
  (*)    (F24 x) (F24 y) = F24 $ Raw.mul 16777259 x y  
  abs    x = x
  signum _ = toF24 1  

instance Fractional F24 where
  recip (F24 x)         = F24 $ Raw.inv 16777259 x
  (/)   (F24 x) (F24 y) = F24 $ Raw.div 16777259 x y
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)  

isZeroF24 :: F24 -> Bool
isZeroF24 (F24 x) = (x == 0)

powF24 :: F24 -> Integer -> F24
powF24 (F24 x) n = F24 $ Raw.pow' 16777259 x n

powF24_ :: F24 -> Int -> F24
powF24_ (F24 x) n = F24 $ Raw.pow 16777259 x (fromIntegral n)

-------------------------------------------------------------------------------
