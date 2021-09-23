
module Test (run) where

import Asm
import Testing (test)
import qualified Examples as X
import qualified Testing (run)
import qualified Primes (primes)

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
      Emit [LIX, IMM done, JXU]
      vars <- Here
      Emit [IMM 42]
      done <- Here
      Emit [HLT]
  test
    asm1 9 [42]

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

  test [LIA,IMM 33,LIX,IMM 7,JXZ,OUT,HLT,LIA,IMM 44,OUT,HLT] 9  [33] -- jump taken
  test [LIA,IMM 0, LIX,IMM 7,JXZ,OUT,HLT,LIA,IMM 44,OUT,HLT] 11 [44] -- jump not taken

  test X.variousInstructions 61 [75,74,111,111,222,77]
  test X.countdown5to0 63 [5,4,3,2,1,0]
  test X.multiply5by7 179 [35]
  test X.fibA 491 [1,1,2,3,5,8,13,21,34,55,89]
  test X.fibB 339 [1,1,2,3,5,8,13,21,34,55,89]
  test X.fibC 209 [1,1,2,3,5,8,13,21,34,55,89,144,233]

  let
    -- example which outputs memory sequentially
    dis = assemble $ mdo
      Emit [LIA, IMM array]
      loop <- Here
      Emit [TAB, TAX, LXA]
      Emit [LIX, IMM done, JXZ]
      Emit [OUT]
      Emit [LIA, IMM 1, ADD]
      Emit [LIX, IMM loop, JXU]
      done <- Here
      Emit [HLT]
      array <- Here
      Emit (map IMM [2,3,5,7,11,13,0])
  test
    dis 133 [2,3,5,7,11,13]

  let
    countup = assemble $ mdo
      Emit [LIB, IMM 1]
      Emit [LIA, IMM 250]
      loop <- Here
      Emit [LIX, IMM done, JXZ, OUT, ADD]
      Emit [LIX, IMM loop, JXU]
      done <- Here
      Emit [HLT]
  test
    countup 81 [250,251,252,253,254,255]

  let
    countup10 = assemble $ mdo
      Emit [LIB, IMM 10]
      Emit [LIA, IMM 200]
      loop <- Here
      Emit [OUT, ADD, LIX, IMM done, JXC]
      Emit [LIX, IMM loop, JXU]
      done <- Here
      Emit [HLT]
  test
    countup10 73 [200,210,220,230,240,250]

  let
    divides numer denom = assemble $ mdo
      la numer
      lb denom
      loop <- Here
      sub
      jz yes
      jc loop
      --no:
      outi 0; halt
      yes <- Here
      outi 1; halt

  test (divides 8 1) 83 [1]
  test (divides 8 2) 43 [1]
  test (divides 8 3) 37 [0]
  test (divides 8 4) 23 [1]
  test (divides 8 5) 27 [0]
  test (divides 8 11) 17 [0]

  -- test prime generation program
  test Primes.primes 192339
    [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97
    ,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199
    ,211,223,227,229,233,239,241,251]

