
module Op (Op(..), Byte) where

import Data.Word8 (Word8)

type Byte = Word8

data Op
  = FETCH -- IR := M[pc], pc++
  -- load immediate
  | LIA -- A := M[pc], pc++
  | LIB -- B := M[pc], pc++
  | LIX -- X := M[pc], pc++
  -- load indexed
  | LXA -- A := M[X]
  | LXB -- B := M[X]
  | LXX -- X := M[X]
  -- Store indexed
  | SXA -- M[X] := A
  -- Jumps
  | JIU -- pc := M[pc]
  | JIZ -- pc := zero(A) ? M[pc] : pc+1
  | JXU -- pc := M[X]                           -- TODO: JXU/JXZ not used so kill
  | JXZ -- pc := zero(A) ? M[X] : pc
  | JAU -- pc := A
  -- Arithmetic
  | ADD -- A := A+B
  | SUB -- A := A-B
  | ADX -- X := A+B
  | SUX -- X := A-B
  | ADDM -- M[X] := A+B
  -- Register transfers
  | TAB -- B := A
  | TAX -- X := A
--  | TXA -- A := X -- TODO: TXA
  -- Output
  | OUT -- Out := A
  | OUTM -- Out := M[X]
  -- Misc
  | HLT
  | IMM Byte
  deriving Show
