
-- | The prime field with prime @p = 2^16 + 1 = 65537@

 module R1CS.Algebra.TestField16 where

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

instance Field F16 where
  fieldName  _ = "F16"
  fieldPrime _ = fromIntegral fieldPrimeF16
  showF      =show . unF16
  pow        = powF16_
  power      = powF16
  isZero     = isZeroF16
  fieldSqrt1 = fieldSqrtF16
  toF        = \x -> F16 (fromInteger $ mod x 65537)
  fromF      = fromIntegral . unF16

instance SmallField F16 where
  toF_   = toF16
  fromF_ = unF16

{-# SPECIALISE fieldSqrt :: F16-> Sqrt F16 #-}

--------------------------------------------------------------------------------

fieldPrimeF16 :: Int
fieldPrimeF16 = 65537

sqrtTable :: Array Word64 (Maybe F16)
sqrtTable = accumArray (flip const) Nothing (0,fromIntegral fieldPrimeF16 - 1) 
  [ (unF16 (x*x), Just x) | k<-[0..fieldPrimeF16-1], let x = F16 (fromIntegral k) ]

fieldSqrtF16 :: F16 -> Maybe F16
fieldSqrtF16 (F16 k) = sqrtTable ! k

--------------------------------------------------------------------------------

-- | prime field with @p = 65537 = 2^16 + 1@
newtype F16 = F16 Word64 deriving (Eq,Ord,Show)

unF16 :: F16 -> Word64
unF16 (F16 x) = x

toF16 :: Int64 -> F16
toF16 = F16 . modF16

-- WARNING: fromInteger breaks if this is Word64, because it first converts
-- a negative integer to Word64, then takes modulo........ 
modF16 :: Int64 -> Word64
modF16 x = fromIntegral (mod x 65537)

instance Num F16 where
  fromInteger = toF16 . fromInteger
  negate (F16 x)         = F16 $ Raw.neg 65537 x
  (+)    (F16 x) (F16 y) = F16 $ Raw.add 65537 x y
  (-)    (F16 x) (F16 y) = F16 $ Raw.sub 65537 x y
  (*)    (F16 x) (F16 y) = F16 $ Raw.mul 65537 x y  
  abs    x = x
  signum _ = toF16 1  

instance Fractional F16 where
  recip (F16 x)         = F16 $ Raw.inv 65537 x
  (/)   (F16 x) (F16 y) = F16 $ Raw.div 65537 x y
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)  

isZeroF16 :: F16 -> Bool
isZeroF16 (F16 x) = (x == 0)

powF16 :: F16 -> Integer -> F16
powF16 (F16 x) n = F16 $ Raw.pow' 65537 x n

powF16_ :: F16 -> Int -> F16
powF16_ (F16 x) n = F16 $ Raw.pow 65537 x (fromIntegral n)

-------------------------------------------------------------------------------
