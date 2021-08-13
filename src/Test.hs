
module Test (run) where

import Asm
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
  test [LIA,IMM 100,LIB,IMM 200,ADD,OUT,HLT] 9 [44] -- addition wraps mod 256
  test [LIA,IMM 100,LIB,IMM 200,ADDOUT,OUT,HLT] 9 [44,100]

  test [LIA,IMM 2,LIB,IMM 3,ADD,OUT,HLT] 9 [5]
  test [LIA,IMM 2,LIB,IMM 3,SUB,OUT,HLT] 9 [255] -- subtraction wraps mod 256

  test [LIA,IMM 33,JIZ,IMM 6,OUT,HLT,LIA,IMM 44,OUT,HLT] 7 [33] -- jump taken
  test [LIA,IMM 0, JIZ,IMM 6,OUT,HLT,LIA,IMM 44,OUT,HLT] 9 [44] -- jump not taken

  test X.variousInstructions 61 [75,74,111,111,222,77]
  test X.countdown5to0 45 [5,4,3,2,1,0]
  test X.multiply5by7 145 [35]
  test X.fibA 447 [1,1,2,3,5,8,13,21,34,55,89]
  test X.fibB 295 [1,1,2,3,5,8,13,21,34,55,89]
  test X.fibC 155 [1,1,2,3,5,8,13,21,34,55,89,144,233]

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

  let
    countup = assemble $ mdo
      Emit [LIB, IMM 1]
      Emit [LIA, IMM 250]
      loop <- Here
      Emit [JIZ, IMM done, OUT, ADD]
      Emit [JIU, IMM loop]
      done <- Here
      Emit [HLT]
  test
    countup 55 [250,251,252,253,254,255]

  let
    countup10 = assemble $ mdo
      Emit [LIB, IMM 10]
      Emit [LIA, IMM 200]
      loop <- Here
      Emit [OUT, ADD, JIV, IMM done]
      Emit [JIU, IMM loop]
      done <- Here
      Emit [HLT]
  test
    countup10 51 [200,210,220,230,240,250]

  let
    divides numer denom = assemble $ mdo
      la numer
      lb denom
      loop <- Here
      sub
      jz yes
      jv no
      jump loop
      yes <- Here
      outi 1; halt
      no <- Here
      outi 0; halt

  test (divides 8 1) 67 [1]
  test (divides 8 2) 35 [1]
  test (divides 8 3) 29 [0]
  test (divides 8 4) 19 [1]
  test (divides 8 5) 21 [0]
  test (divides 8 11) 13 [0]
