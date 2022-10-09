
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
  , countdownForeverC
  , module Primes

  , rom1, rom2
  , table
  ) where

import Asm
import Primes

rom1 :: [(String,[Op])] -- programs which don't need memory
rom1 =
  [
    -- Shifter-unit only
    ("shiftyR",shiftyR) -- ASR
  , ("triangleShift",triangleShift) -- LSR

    -- ALU only
  , ("fibs",fibsNew) -- ADD
  , ("open-count-loop", openCountLoop)
  , ("tight-count-loop", tightCountLoop)
  , ("shiftyL",shiftyL) -- ADC
  , ("countdownForeverC",countdownForeverC) -- SUB

    -- ALU and Shifter
  , ("knightRider",knightRider) -- (T)ADD, (T)LSR
  ]


rom2 :: [(String,[Op])] -- programs which *DO* need memory
rom2 =
  [ ("primes",primes)
  , ("primesViaShift",primesViaShift)
  , ("primesSieve",primesSieve)
  , ("primesSieve2",primesSieve2)
  , ("collatz",collatz)
  , ("multiply16bit",multiply16bit)
  , ("varProg0init",varProg0init)
  ]


table :: [(String,[Op])]
table = rom1 ++ rom2


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


initVar :: Byte -> Byte -> Asm Byte
initVar var v = do
  la v; storeA var
  pure var

multiply5by7 :: [Op]
multiply5by7 = assemble $ mdo
  left <- initVar 0xff 5
  right <- initVar 0xfe 7
  result <- initVar 0xfd 0
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
  finish <- Here
  loadA result
  out
  spin

fibA :: [Op]
fibA = assemble $ mdo
  v1 <- initVar 0xff 0
  v2 <- initVar 0xfe 1
  v3 <- initVar 0xfd 0
  loop <- Here
  loadB v1
  loadA v2; out
  add; storeA v3
  lb 144; sub; jz done
  loadA v2; storeA v1
  loadA v3; storeA v2
  jump loop
  done <- Here
  spin

fibB :: [Op]
fibB = assemble $ mdo
  p <- initVar 0xff 0
  q <- initVar 0xfe 1
  loop <- Here
  loadA q
  out
  lb 89; sub; jz done; add
  loadB p
  storeAdd q
  storeA p
  jump loop
  done <- Here
  spin

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
  p <- initVar 0xff 0
  q <- initVar 0xfe 0
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

fib3vars :: [Op] -- program which uses memory for variable storage (3 vars)
fib3vars = assemble $ mdo
  p <- initVar 0xff 0
  q <- initVar 0xfe 0
  r <- initVar 0xfd 0
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

varProg1 :: [Op] -- small program which does something useful with memory
varProg1 = assemble $ mdo
  v1 <- initVar 0xff 42
  start <- Here
  loadA v1
  out
  lb 1
  add
  storeA v1
  la 0
  out
  jump start

varProg0 :: [Op] -- even smaller
varProg0 = assemble $ mdo
  v1 <- initVar 0xff 17
  lb 1
  loop <- Here
  loadA v1
  out
  add
  --storeA v1
  sxa
  jump loop

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
  jump loop

collatz :: [Op]
collatz = assemble $ mdo

  -- variable locations
  let next = 0xff
  let current = 0xfe
  let steps = 0xfd
  let count = 0xfc
  let tmp = 0xfb

  storeI 3 next

  main <- Here
  loadA next
  storeA current
  lb 1; add; storeA next

  lza; storeA steps
  sequence <- Here
  increment steps 1
  loadA current
  out
  lb 1; sub; jz reached1; add

  -- try to divide by 2
  storeA tmp
  lza; storeA count
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

  -- multiply by 3 and +1
  mult3plus1 <- Here
  loadA current
  tab; add; add; lb 1; add
  storeA current
  jump sequence

  reached1 <- Here
  loadA steps
  out
  jump main


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
  jxu

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

-- new example to try the planned shifter unit
-- we can only shift right; storing the shifted-out bit as a flag
-- we can shift-in (asr) or not (lsr) the previous shifted-out bit
shiftyR :: [Op]
shiftyR = assemble $ do
  la 64
  loop <- Here
  out
  asr
  jump loop

triangleShift :: [Op]
triangleShift = assemble $ do
  la 0xff
  loop <- Here
  out
  lsr
  jump loop


-- fibs example which detects 8-bit overflow and restarts

fibsNew :: [Op]
fibsNew = assemble $ mdo
  lx start
  start <- Here
  la 1
  out
  tab
  loop <- Here
  add
  jxc
  out
  addb
  jxc
  outb
  jump loop

shiftyL :: [Op]
shiftyL = assemble $ do
  la 1
  loop <- Here
  out
  tab
  adc
  jump loop


knightRider :: [Op]
knightRider = assemble $ mdo

  la 3
  out

  goLeft <- Here
  tab
  tadd
  jc goRight
  add
  out
  jump goLeft

  goRight <- Here
  tlsr
  js goLeft
  lsr
  out
  jump goRight


-- 16 bit multiplication. needs memory.
multiply16bit :: [Op]
multiply16bit = assemble $ mdo
  -- example: 201 * 103 = 20703
  -- hex: c9 * 67 =  (dec:80,223) 50,df

  let (acc0,acc1,lhs0,lhs1,right) = (0xf1,0xf2,0xf3,0xf4,0xf5)

  storeI 201 lhs0
  storeI 103 right

  zerom acc0
  zerom acc1
  zerom lhs1
  jump shiftRightRhs

  check_continue <- Here
  lx right; lxa
  jz finished
  --jump shiftLeftLhs

  --shiftLeftLhs <- Here
  lx lhs0; lxa; tab; add_store
  lx lhs1; lxa; tab; adc_store

  --jump shiftRightRhs
  shiftRightRhs <- Here
  lx right; lxa; lsr_store
  js accumulate
  jump check_continue

  accumulate <- Here
  lx lhs0; lxa; lx acc0; lxb; add_store
  lx lhs1; lxa; lx acc1; lxb; adc_store
  jump check_continue

  finished <- Here
  outm acc1
  outm acc0
  spin


-- for Asm.hs...
add_store :: Asm ()
add_store = Emit [ADDM]

adc_store :: Asm ()
adc_store = Emit [ADCM]

lsr_store :: Asm ()
lsr_store = Emit [LSRM]

zerom :: Byte -> Asm ()
zerom loc = Emit [LIX, IMM loc, SXZ]
