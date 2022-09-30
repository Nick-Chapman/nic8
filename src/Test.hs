
module Test (run) where

import Asm
import Testing (test)
import qualified Examples as X
import qualified Testing (run)
import qualified Primes

run :: IO ()
run = Testing.run $ do

  let test' code n xs = test (assemble $ do Emit code; spin) n xs

  test' [] 1 []
  test' [NOP] 2 []
  test' [NOP,NOP] 3 []
  test' [ADD] 2 []
  test' [OUT] 2 [0]

  test' [LIA,IMM 42,OUT] 4 [42]
  test' [LIA,IMM 100,LIB,IMM 200,ADD,OUT] 7 [44] -- addition wraps mod 256
  test' [LIA,IMM 100,LIB,IMM 200,ADDOUT,OUT] 7 [44,100]

  test' [LIA,IMM 2,LIB,IMM 3,ADD,OUT] 7 [5]
  test' [LIA,IMM 2,LIB,IMM 3,SUB,OUT] 7 [255] -- subtraction wraps mod 256

  -- 0xff executes as a 2-cycle NOP
  test' [IMM 0xff] 2 []
  test' [IMM 0xff, IMM 0xff] 3 []
  test' [IMM 0xff, IMM 0xff, IMM 0xff] 4 []

  let
    -- 1st example to use the assembler
    asm1 = assemble $ mdo
      Emit [LIX, IMM vars]
      Emit [OUTM]
      Emit [LIX, IMM done, JXU]
      vars <- Here
      Emit [IMM 42]
      done <- Here
      spin
  test
    asm1 8 [42]

  let
    -- loop PC around memory before running into a spin
    loopAroundPC = assemble $ mdo
      Emit [LIX]
      codeLocationToMod <- Here
      Emit [IMM after]
      before <- Here
      jxu -- initially this jumps to following instruction
      after <- Here
      la before -- update the target location to form a spin
      storeA codeLocationToMod
      pure ()
  test
    loopAroundPC 258 []

  let
    makeCode i = assemble $ mdo
      la i
      jz done
      out
      spin
      done <- Here
      la 44
      out
      spin
  test (makeCode 33) 6 [33]
  test (makeCode 0) 8 [44]

  test X.variousInstructions 44 [75,74,111,111,222,77]
  test X.countdown5to0 48 [5,4,3,2,1,0]
  test X.multiply5by7 117 [35]
  test X.fibA 319 [1,1,2,3,5,8,13,21,34,55,89]
  test X.fibB 222 [1,1,2,3,5,8,13,21,34,55,89]
  test X.fibC 107 [1,1,2,3,5,8,13,21,34,55,89,144,233]

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
      spin
      array <- Here
      Emit (map IMM [2,3,5,7,11,13,0])
  test
    dis 94 [2,3,5,7,11,13]

  let
    countup = assemble $ mdo
      Emit [LIB, IMM 1]
      Emit [LIA, IMM 250]
      loop <- Here
      Emit [LIX, IMM done, JXZ, OUT, ADD]
      Emit [LIX, IMM loop, JXU]
      done <- Here
      spin
  test
    countup 63 [250,251,252,253,254,255]

  let
    countup10 = assemble $ mdo
      Emit [LIB, IMM 10]
      Emit [LIA, IMM 200]
      loop <- Here
      Emit [OUT, ADD, LIX, IMM done, JXC]
      Emit [LIX, IMM loop, JXU]
      done <- Here
      spin
  test
    countup10 56 [200,210,220,230,240,250]

  let
    divides numer denom = assemble $ mdo
      la numer
      lb denom
      loop <- Here
      sub
      jz yes
      jc loop
      --no:
      outi 0; spin
      yes <- Here
      outi 1; spin

  test (divides 8 1) 45 [1]
  test (divides 8 2) 25 [1]
  test (divides 8 3) 22 [0]
  test (divides 8 4) 15 [1]
  test (divides 8 5) 17 [0]
  test (divides 8 11) 12 [0]

  -- test prime generation program
  let spining = True
  test (Primes.primes spining) 103114
    [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97
    ,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199
    ,211,223,227,229,233,239,241,251]

  let newPrimes = Primes.primesViaShift
  test newPrimes 34177
    [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97
    ,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199
    ,211,223,227,229,233,239,241,251]

