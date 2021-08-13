
module Primes (outputPrimes) where -- program to compute and output prime numbers

import Asm

outputPrimes :: [Op]
outputPrimes = assemble $ mdo

  outi 2
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
  jv noDiv
  jump loop_subtract
  noDiv <- Here
  -- move to next prime
  increment primePtr 1
  jump loop_testPrimes

  -- candidate does not divide by any previous prime...
  noMorePrimes <- Here
  loadA candidate
  out -- so output
  loadX primePtr; sxa -- and save

  -- candidate is a multiple, so try next candidate
  divides <- Here
  increment candidate 2 -- simple optimization; step by 2
  jv done -- stop if candidate exceeds 255
  jump loop_testCandidate -- otherwise test next candidate
  done <- Here
  halt

  -- Variables
  candidate <- variable 3
  primePtr <- variable allPrimes

  -- Primes saved here; zero byte terminates list
  allPrimes <- Here
  pure ()
