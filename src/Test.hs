
module Test (run) where

import Asm (Asm(..),assemble)
import Op (Op(..))
import Testing (test)
import qualified Examples as X
import qualified Testing (run)

run :: IO ()
run = Testing.run $ do
  test [HLT] 1 []
  test [NOP,HLT] 2 []
  test [NOP,NOP,HLT] 3 []
  test [ADD,HLT] 3 []
  test [OUT,HLT] 3 [0]

  let
    -- 1st example to use the assembler
    asm1 = assemble $ mdo
      Emit [LIX, IMM vars]
      Emit [OUTM]
      Emit [JIU, IMM done]
      vars <- Here
      Emit [IMM 42]
      done <- Here
      Emit [HLT]
  test
    asm1 7 [42]

  let
    -- example which loops around PC and then runs into halt instruction
    loopAroundPC = assemble $ mdo
      a <- Here
      Emit [LIX, IMM b, LXA, LIX, IMM a, SXA] -- copy a HLT instruction from b to a
      Emit [LIX, IMM b, LIA, IMM 0, SXA] -- then overwrite the original HLT with 0 (== NOP/Fetch)
      b <- Here
      Emit [HLT]
      pure ()
  test
    loopAroundPC 260 []

  test [LIA,IMM 42,OUT,HLT] 5 [42]
  test [LIA,IMM 100,LIB,IMM 200,ADD,OUT,HLT] 9 [44]
  test [LIA,IMM 100,LIB,IMM 200,ADDOUT,OUT,HLT] 9 [44,100]
  test X.variousInstructions 61 [75,74,111,111,222,77]
  test X.countdown5to0 45 [5,4,3,2,1,0]
  test X.multiply5by7 145 [35]
  test X.fibA 447 [1,1,2,3,5,8,13,21,34,55,89]
  test X.fibB 295 [1,1,2,3,5,8,13,21,34,55,89]
  test X.fibC 47 [1,1,2,3,5,8,13,21,34,55,89]

  let
    -- example with accesses memory sequentially via incrementing X
    dis = assemble $ mdo
      Emit [LIB, IMM 1]
      Emit [LIX, IMM array]
      loop <- Here
      Emit [LXA]
      Emit [JIZ, IMM done]
      Emit [OUT]
      Emit [TXA, ADDX]
      Emit [JIU, IMM loop]
      done <- Here
      Emit [HLT]
      array <- Here
      Emit (map IMM [2,3,5,7,11,13,0])
  test
    dis 81 [2,3,5,7,11,13]

  -- TODO: z-flag is sep from Acc (need imp change)
  -- TODO: imp/test 4 jump conditions: unconditional, Z, Pos, Neg (using flags: Z/Overflow)
