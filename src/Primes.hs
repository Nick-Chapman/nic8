
module Primes (primes,primesViaShift,primesSieve,primesSieve2) where

import Data.Bits (shiftL)

import Asm hiding (jz)
import qualified Asm

jz :: Byte -> Asm ()
jz loc = do Asm.jz loc --; lx 99

primes :: [Op]
primes = assemble $ mdo

  -- Variables
  let candidate = 0x81
  let primePtr = 0x82

  -- Primes saved here; zero byte terminates list
  let allPrimes = 0x83

  outi 2 -- output first prime 2 (special case)
  storeI 3 candidate -- set first candidiate as 3
  la 0; lx allPrimes; sxa -- zero first cell in saved primes

  loop_testCandidate <- Here

  -- Test if candidate is divisible by any prime found so far
  la allPrimes
  storeA primePtr

  -- Test if candidate is divible by next prime
  loop_testPrimes <- Here
  tax; lxx; txa
  jz noMorePrimes
  tab
  loadA candidate
  loop_subtract <- Here
  sub

  jz divides
  jc loop_subtract
  -- noDiv: move to next prime
  increment primePtr 1
  jump loop_testPrimes

  -- candidate does not divide by any previous prime... so it is must be prime
  noMorePrimes <- Here
  loadA candidate
  out -- output it!
  loadX primePtr; sxa -- and save!
  txa; lb 1; addx; la 0; sxa -- zero next cell in saved primes

  -- candidate is a multiple, so try next candidate
  divides <- Here
  increment candidate 2 -- simple optimization; step by 2
  jc done -- restart if candidate exceeds 255
  jump loop_testCandidate -- otherwise test next candidate

  done <- Here
  spin


----------------------------------------------------------------------
-- divisibility checking using shift...
-- used for new imp of primes

primesViaShift :: [Op]
primesViaShift = assemble $ mdo

  outi 2
  outi 3
  outi 5
  outi 7
  outi 11
  outi 13

  let i = 0xE1 -- we need only a single variable / memory-slot

  la 15 -- start at 15
  storeA i

  again <- Here

  let
    divTest n yes no = mdo
      lb n
      loadA i
      loop <- Here
      tsub
      jc fits
      jump next
      fits <- Here
      sub
      jz yes
      next <- Here
      tax
      tba
      lsrb
      txa
      js no
      jump loop

  divTest (3 `shiftL` 6) incThenAgain dontDivide3
  dontDivide3 <- Here

  divTest (5 `shiftL` 5) incThenAgain dontDivide5
  dontDivide5 <- Here

  divTest (7 `shiftL` 5) incThenAgain dontDivide7
  dontDivide7 <- Here

  divTest (11 `shiftL` 4) incThenAgain dontDivide11
  dontDivide11 <- Here

  divTest (13 `shiftL` 4) incThenAgain dontDivide13
  dontDivide13 <- Here

  -- print the new prime
  loadA i
  out

  -- setup to test the next candidate
  incThenAgain <- Here
  loadA i
  lb 2 -- step by 2
  add
  storeA i
  jc doSpin
  jump again

  doSpin <- Here
  spin


----------------------------------------------------------------------
-- Now we have lots of memory!
-- Let's code the real sieve algorth. Expect it to be quite quick.

primesSieve :: [Op]
primesSieve = assemble $ mdo
  lb 1
  la 0
  initPass <- Here
  tax
  sxa
  add
  jc initDone
  jump initPass
  initDone <- Here

  let
    sieveN :: Byte -> Asm ()
    sieveN n = mdo
      la n
      tab
      sieve2 <- Here
      add
      jc sieveDone
      tax
      lza
      sxa
      txa
      jump sieve2
      sieveDone <- Here
      pure ()

  sieveN 2
  sieveN 3
  sieveN 5
  sieveN 7
  sieveN 11
  sieveN 13

  lb 1
  la 2
  outputPass <- Here
  tax
  lxa
  jz dontOutput
  out
  dontOutput <- Here
  txa
  add
  jc outputDone
  jump outputPass
  outputDone <- Here

  spin


----------------------------------------------------------------------
-- version of seive which does not have the first 6 primes hardcoded into it
-- and repicate the sieving code 6 times.
-- so we trade some speed for less rom

primesSieve2 :: [Op]
primesSieve2 = assemble $ mdo

  lb 1
  la 0
  initPass <- Here
  tax; sxa --init every mem slot to itself
  add
  jc initDone
  jump initPass
  initDone <- Here

  la 2 -- from 2
  again <- Here
  lzx; sxa -- save a in mem[0]
  tax; lxa -- look in mem[a], find either a or 0
  jz sieveDone -- dont output or sieve if zero
  out -- a

  -- sieve a (zero all mem slots when are a multiple of a)
  tab
  sieve <- Here
  add
  jc sieveDone
  tax
  sxz
  jump sieve
  sieveDone <- Here

  lzx; lxa -- restore a
  lb 1; add -- increment it
  jc outputDone
  jump again
  outputDone <- Here

  spin
