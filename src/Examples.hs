
module Examples
  ( variousInstructions
  , countdown5to0
  , multiply5by7
  , fibA, fibB
  ) where

import Op (Op(..))

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
  , ADX
  , LXA
  , OUT -- expect 222
  , LIA, IMM 0
  , LIX, IMM 51
  , LXX
  , LXA
  , OUT -- expect 77
  , HLT
  ]

countdown5to0 :: [Op]
countdown5to0 =
  [ LIB, IMM 1
  , LIA, IMM 5
--4:
  , OUT
  , SUB
  , JIZ, IMM 10
  , JIU, IMM 4
--10:
  , OUT
  , HLT
  ]

multiply5by7 :: [Op]
multiply5by7 =
  [ JIU, IMM 10
-- static mem storage
  , IMM 5 -- #2 left
  , IMM 7 -- #3 right
  , IMM 0 -- #4 result
-- 5: finish
  , LIX, IMM result, LXA
  , OUT
  , HLT
-- 10: main loop
  , LIX, IMM left, LXA -- , OUT
  , JIZ, IMM 5
  , LIB, IMM 1
  , SUB
  , SXA
  , LIX, IMM right, LXB
  , LIX, IMM result, LXA
  , ADD --, OUT
  , SXA
  , JIU, IMM 10
  ]
  where
    left = 2
    right = 3
    result = 4

fibA :: [Op]
fibA =
  [ JIU, IMM 6
-- 2:
  , HLT
  , IMM 0
  , IMM 1
  , IMM 0
-- 6:
  , LIX, IMM 3, LXA, TAB
  , LIX, IMM 4, LXA
  , ADD, LIX, IMM 5, SXA, OUT
  , LIB, IMM 55, SUB, JIZ, IMM 2
  , LIX, IMM 4, LXA, LIX, IMM 3, SXA
  , LIX, IMM 5, LXA, LIX, IMM 4, SXA
  , JIU, IMM 6
  ]

fibB :: [Op] -- smaller code
fibB =
  [ JIU, IMM 5
-- 2:
  , HLT
  , IMM 0
  , IMM 1
-- 5:
  , LIX, IMM 4, LXA, OUT
  , LIB, IMM 233, SUB, JIZ, IMM 2, ADD
  , LIX, IMM 3, LXB
  , LIX, IMM 4, ADDM
  , LIX, IMM 3, SXA
  , JIU, IMM 5
  ]
