
module Test (run) where

import OpEmu (Op(..))
import Testing (test)
import qualified Examples as X
import qualified Testing (run)

run :: IO ()
run = Testing.run $ do
  test [OUT,HLT] [0]
  test [LIA,IMM 42,OUT,HLT] [42]
  test X.variousInstructions [75,74,111,222,77]
  test X.countdown5to0 [5,4,3,2,1,0]
  test X.multiply5by7 [35] -- 5*7
  test X.fibA [  1,2,3,5,8,13,21,34,55] -- fib
  test X.fibB [1,1,2,3,5,8,13,21,34,55,89,144,233] -- fib (smaller code)
