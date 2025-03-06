
-- | Testing some simple example circuits
--

module CircomLib.Main where

--------------------------------------------------------------------------------

import R1CS -- .Misc ( Verbosity(..) )

import qualified R1CS.Test.Spec as Spec

import qualified CircomLib.Lib.Switcher             as Switcher
import qualified CircomLib.Lib.ForceEqualIfEnabled  as ForceEqualIfEnabled
import qualified CircomLib.Lib.Comparators          as Comparators
import qualified CircomLib.Lib.MultiAND             as MultiAND
import qualified CircomLib.Lib.MultiMux             as MultiMux

--------------------------------------------------------------------------------

testCircomLib :: IO ()
testCircomLib = testCircomLib' Field20 Silent

testCircomLib' :: FieldChoice -> Verbosity -> IO ()
testCircomLib' field verbosity = runWithField field $ \pxy ->  do
  
  let runSpec     what = Spec.testSemantics     pxy what verbosity
  let runSpecMany what = Spec.testSemanticsMany pxy what verbosity
  
  runSpec     Switcher.spec
  runSpec     ForceEqualIfEnabled.spec
  runSpecMany Comparators.specs                     
  runSpecMany MultiAND.specs
  runSpecMany MultiMux.specs
 
--------------------------------------------------------------------------------
