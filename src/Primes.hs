
module Primes (primes,primesViaShift) where

import Data.Bits (shiftL)

import Asm hiding (jz)
import qualified Asm

jz :: Byte -> Asm ()
jz loc = do Asm.jz loc --; lx 99

primes :: Bool -> [Op]
primes spining = assemble $ mdo

  -- Variables
  let candidate = 0x81
  let primePtr = 0x82

  -- Primes saved here; zero byte terminates list
  let allPrimes = 0x83

  start <- Here
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

  -- explore moving between B and X... hmm, something seems wrong
{-
  tbx
  --lb 99
  txb -- This should have no effect following a tbx, but it does
-}

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
  if spining then spin else pure ()
  jump start

  pure ()


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

  let i = 0xE1

  la 3 -- start at 3
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

  --jump doPrint


  --doPrint <- Here
  loadA i
  out
  jump incThenAgain

  incThenAgain <- Here
  loadA i
  lb 2 -- step by 2
  add
  storeA i
  jc spin
  jump again

  spin <- Here
  jump spin
