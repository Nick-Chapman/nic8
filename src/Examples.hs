
module Examples
  ( variousInstructions
  , countdown5to0
  , multiply5by7
  , fibA, fibB, fibC,
  ) where

import Asm

variousInstructions :: [Op]
variousInstructions =
  [ LIA, IMM 42
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
  , HLT
  ]

-- TODO: recode remaining example using Asm

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
  , HLT
  ]

multiply5by7 :: [Op]
multiply5by7 =
  [ LIX, IMM 11, JXU
-- static mem storage
  , IMM 5 -- #3 left
  , IMM 7 -- #4 right
  , IMM 0 -- #5 result
-- 6: finish
  , LIX, IMM result, LXA
  , OUT
  , HLT
-- 11: main loop
  , LIX, IMM left, LXA
  , LIX, IMM 6, JXZ
  , LIB, IMM 1
  , SUB
  , LIX, IMM left, SXA
  , LIX, IMM right, LXB
  , LIX, IMM result, LXA
  , ADD
  , SXA
  , LIX, IMM 11, JXU
  ]
  where
    left = 3
    right = 4
    result = 5

fibA :: [Op]
fibA =
  [ LIX, IMM 7, JXU
-- 3:
  , HLT
  , IMM 0
  , IMM 1
  , IMM 0
-- 7:
  , LIX, IMM 4, LXA, TAB
  , LIX, IMM 5, LXA, OUT
  , ADD, LIX, IMM 6, SXA
  , LIB, IMM 144, SUB, LIX, IMM 3, JXZ
  , LIX, IMM 5, LXA, LIX, IMM 4, SXA
  , LIX, IMM 6, LXA, LIX, IMM 5, SXA
  , LIX, IMM 7, JXU
  ]

fibB :: [Op]
fibB = assemble $ mdo
  jump loop
  done <- Here
  halt
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
  jv done
  jump loop
  done <- Here
  halt
