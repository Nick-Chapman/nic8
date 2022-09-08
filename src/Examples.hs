
module Examples
  ( variousInstructions
  , countdown5to0
  , multiply5by7
  , fibA, fibB, fibC
  , vSmall
  , fibForever, fibUnrolled
  , fib2vars, fib3vars
  , varProg1, varProg0
  , collatz
  , openCountLoop, tightCountLoop
  , countdownForeverZ, countdownForeverC
  , module Primes

  , table
  ) where

import Asm
import Primes

table :: [(String,[Op])]
table =
  [ ("open-count-loop", openCountLoop)
  , ("tight-count-loop", tightCountLoop)
  , ("fibs-forever",fibForever)
  , ("countdownForeverZ",countdownForeverZ)
  , ("countdownForeverC",countdownForeverC)
  , ("varProg0init",varProg0init)
  , ("primes",primes False)
  , ("collatz",collatz)
  ]

variousInstructions :: [Op]
variousInstructions = assemble $ do
 Emit [ LIA, IMM 42
  , LIB, IMM 33
  , ADD
  , OUT -- expect 75
  , LIB, IMM 1
  , SUB
  , OUT -- expect 74
  , LIX, IMM 50
  , LIA, IMM 111
  , SXA
  , LIX, IMM 51
  , LIA, IMM 222
  , SXA
  , TAX
  , LIA, IMM 77
  , SXA
  , LIX, IMM 50
  , LXA
  , OUT -- expect 111
  , LIA, IMM 50
  , OUTM -- expect 111
  , LIB, IMM 1
  , ADDX
  , LXA
  , OUT -- expect 222
  , LIA, IMM 0
  , LIX, IMM 51
  , LXX
  , LXA
  , OUT -- expect 77
  ]
 spin


countdown5to0 :: [Op]
countdown5to0 =
  [ LIB, IMM 1
  , LIA, IMM 5
--4:
  , OUT
  , SUB
  , LIX, IMM 12, JXZ
  , LIX, IMM 4, JXU
--12:
  , OUT
  , LIX, IMM 15
--15:
  , JXU
  ]

multiply5by7 :: [Op]
multiply5by7 = assemble $ mdo
  jump mainLoop
  left <- variable 5
  right <- variable 7
  result <- variable 0
  finish <- Here
  loadA result
  out
  spin
  mainLoop <- Here
  loadA left
  jz finish
  lb 1
  sub
  storeA left
  loadB right
  loadA result
  add
  sxa
  jump mainLoop

fibA :: [Op]
fibA = assemble $ mdo
  jump loop
  done <- Here
  spin
  v1 <- variable 0
  v2 <- variable 1
  v3 <- variable 0
  loop <- Here
  loadB v1
  loadA v2; out
  add; storeA v3
  lb 144; sub; jz done
  loadA v2; storeA v1
  loadA v3; storeA v2
  jump loop

fibB :: [Op]
fibB = assemble $ mdo
  jump loop
  done <- Here
  spin
  p <- variable 0
  q <- variable 1
  loop <- Here
  loadA q
  out
  lb 89; sub; jz done; add
  loadB p
  storeAdd q
  storeA p
  jump loop

fibC :: [Op]
fibC = assemble $ mdo
  lb 0
  la 1
  loop <- Here
  out; addx
  tab; txa
  jc done
  jump loop
  done <- Here
  spin

vSmall :: [Op]
vSmall = assemble $ do
  la 21
  tab
  add
  out
  spin

fibForever :: [Op]
fibForever = assemble $ mdo
  la 1
  loop <- Here
  out; addx
  tab; txa
  jump loop

fibUnrolled :: [Op]
fibUnrolled = assemble $ mdo
  lb 1
  addout
  add
  addout
  addb
  addout
  add
  addout
  addb
  addout
  add
  addout
  addb
  spin

fib2vars :: [Op] -- program which uses memory for variable storage (2 vars)
fib2vars = assemble $ mdo
  start <- Here
  la 1
  storeA q
  la 0
  storeA p
  loop <- Here
  loadB p
  loadA q
  storeA p
  add
  storeA q
  out
  lb 233; sub
  jz start --Emit [TAX, JXZ]
  jump loop
  pure ()
  p <- variable 0
  q <- variable 0
  pure ()

fib3vars :: [Op] -- program which uses memory for variable storage (3 vars)
fib3vars = assemble $ mdo
  start <- Here
  la 0
  storeA p
  la 1
  storeA q
  loop <- Here
  loadA p
  loadB q
  add
  storeA r
  loadA q
  storeA p
  loadA r
  storeA q
  out
  lb 233; sub; jz start
  jump loop
  p <- variable 0
  q <- variable 0
  r <- variable 0
  pure ()

varProg1 :: [Op] -- small program which does something useful with memory
varProg1 = assemble $ mdo
  start <- Here
  loadA v1
  out
  lb 1
  add
  storeA v1
  la 0
  out
  jump start
  v1 <- variable 42
  pure ()

varProg0 :: [Op] -- even smaller
varProg0 = assemble $ mdo
  lb 1
  loop <- Here
  loadA v1
  out
  add
  --storeA v1
  sxa
  jump loop
  v1 <- variable 17
  pure ()

varProg0init :: [Op] -- dyamic memory initialization, for Harvard
varProg0init = assemble $ mdo
  let v1 = 0x99 -- can be anywhere
  lb 1
  lx v1
  la 0x42
  sxa
  loop <- Here
  loadA v1
  --lxa
  out
  add
  --storeA v1
  sxa
  jump loop -- or use jump' !
  --v1 <- variable 0x42
  pure ()

collatz :: [Op]
collatz = assemble $ mdo
  la 3
  storeA next

  main <- Here
  loadA next
  storeA current
  lb 1; add; storeA next

  la 0; storeA steps
  sequence <- Here
  increment steps 1
  loadA current
  out
  lb 1; sub; jz reached1; add

  -- try to divide by 2
  storeA tmp
  la 0; storeA count
  div2 <- Here
  loadA tmp
  jz after_div2
  lb 2; sub
  jc is_div2
  jump mult3plus1 -- not divisible by 2

  is_div2 <- Here
  storeA tmp
  increment count 1
  jump div2
  after_div2 <- Here
  loadA count
  storeA current -- divide by 2
  jump sequence

  count <- variable 0
  tmp <- variable 0

  -- multiply by 3 and +1
  mult3plus1 <- Here
  loadA current
  tab; add; add; lb 1; add
  storeA current
  jump sequence

  reached1 <- Here
  loadA steps; out
  jump main

  next <- variable 3 -- TODO: initialize vars (as no longer in program text)
  current <- variable 0
  steps <- variable 0
  pure ()


openCountLoop :: [Op]
openCountLoop = assemble $ do
  lb 1
  add
  out

tightCountLoop :: [Op]
tightCountLoop = assemble $ mdo
  lb 1
  lx loop
  loop <- Here
  add
  out
  --jump loop
  jxu

countdownForeverZ :: [Op]
countdownForeverZ = assemble $ do
  lb 1
  start <- Here
  la 5
  loop <- Here
  out
  jz start
  sub
  jump' loop -- using new JIU instruction

countdownForeverC :: [Op]
countdownForeverC = assemble $ do
  lb 1
  start <- Here
  la 5
  loop <- Here
  out
  sub
  jc loop
  jump start
