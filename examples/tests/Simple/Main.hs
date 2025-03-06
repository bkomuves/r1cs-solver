
-- | Testing some simple example circuits
--

module Simple.Main where

--------------------------------------------------------------------------------

import R1CS -- ( Verbosity(..) , FieldChoice(..) )

import qualified R1CS.Test.Spec as Spec

import qualified Simple.Misc.IsBoolean   as IsBoolean               
import qualified Simple.Misc.SumOfCubes  as SumOfCubes
import qualified Simple.Misc.StdBasis    as StdBasis
import qualified Simple.Misc.Fibonacci   as Fibonacci
import qualified Simple.Misc.RightShift  as RightShift

--------------------------------------------------------------------------------

testSimple :: IO ()
testSimple = testSimple' Field20 Silent

testSimple' :: FieldChoice -> Verbosity -> IO ()
testSimple' fld verbosity = runWithField fld $ \pxy -> do

  let runSpec     what = Spec.testSemantics     pxy what verbosity
  let runSpecMany what = Spec.testSemanticsMany pxy what verbosity

  runSpec     IsBoolean.spec                     
  runSpec     SumOfCubes.spec       
  runSpec     StdBasis.spec
  runSpecMany Fibonacci.specs

  -- the unsound example:
  Spec.testSemantics pxy RightShift.spec (max Info verbosity)
  RightShift.testHonestProver fld verbosity

--------------------------------------------------------------------------------
