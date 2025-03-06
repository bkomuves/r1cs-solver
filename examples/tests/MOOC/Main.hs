
-- | Testing the components of the 2023 Berkeley ZKP MOOC lab exercises
--
-- See <https://github.com/rdi-berkeley/zkp-mooc-lab>
--
-- Note: the organizers of the MOOC asked not to publish the solutions,
-- bacause they want to reuse the exercises in the future.
--
-- So we only include the tests and the unsolved template circuit here,
-- you have to fill the solutions yourself.
--

module MOOC.Main where

--------------------------------------------------------------------------------

import qualified R1CS.Test.Spec as Spec

import MOOC.Common

import qualified MOOC.Lab.Num2Bits            as Num2Bits               
import qualified MOOC.Lab.LessThan            as LessThan               
import qualified MOOC.Lab.CheckBitLength      as CheckBitLength               
import qualified MOOC.Lab.CheckWellFormedness as CheckWellFormedness               
import qualified MOOC.Lab.RightShift          as RightShift               
import qualified MOOC.Lab.RoundAndCheck       as RoundAndCheck               
import qualified MOOC.Lab.LeftShift           as LeftShift               
import qualified MOOC.Lab.MSNZB               as MSNZB               
import qualified MOOC.Lab.Normalize           as Normalize               
import qualified MOOC.Lab.FloatAdd            as FloatAdd               

--------------------------------------------------------------------------------

testLab :: IO ()
testLab = testLab' Field20 Silent

testLab' :: FieldChoice -> Verbosity -> IO ()
testLab' fld verbosity = runWithField fld $ \pxy -> do
  let runSpec what = Spec.testSemantics pxy what verbosity
  runSpec Num2Bits.spec             
  runSpec LessThan.spec             
  runSpec CheckBitLength.spec       
  runSpec CheckWellFormedness.spec  
  runSpec RightShift.spec           
  runSpec RoundAndCheck.spec        
  runSpec LeftShift.spec            
  runSpec MSNZB.spec                
  runSpec Normalize.spec            

-- | testing @FloatAdd@ is very slow, so we do it separately
testFloatAdd :: IO ()
testFloatAdd = testFloatAdd' Field20 Info -- Silent

testFloatAdd' :: FieldChoice -> Verbosity -> IO ()
testFloatAdd' fld verbosity = runWithField fld $ \pxy -> do
  let runSpecMany what = Spec.testSemanticsMany pxy what verbosity
  runSpecMany FloatAdd.specs

--------------------------------------------------------------------------------
