
module Primes (primes) where -- program to compute and output prime numbers

import Asm

primes :: Bool -> [Op]
primes spining = assemble $ mdo

  start <- Here
  outi 2 -- output first prime 2 (special case)
  la 3; storeA candidate -- set first candidiate as 3
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
  if spining then spin else pure ()
  jump start

  -- Variables
  candidate <- variable 0
  primePtr <- variable 0

  -- Primes saved here; zero byte terminates list
  allPrimes <- Here
  pure ()
