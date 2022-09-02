
module Op (Op(..), Byte) where

import Data.Word8 (Word8)

type Byte = Word8

data Op
  -- nop/fetch -- must be encoded as zero
  = NOP -- IR := M[pc], pc++
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
  | JXU -- pc := X
  | JXZ -- pc := zero(A) ? X : pc+1
  | JXC -- pc := carryFlag ? X : pc+1
  | JIU -- pc := M[pc]
  -- Arithmetic (also sets overflow flag)
  | ADD -- A := A+B
  | ADDB -- B := A+B
  | ADDX -- X := A+B
  | ADDM -- M[X] := A+B
  | ADDOUT -- OUT := A+B
  | SUB -- A := A-B
  | SUBB -- B := A-B
  | SUBX -- X := A-B
  -- more SUBS possible too
  -- Register transfers
  | TAB -- B := A
  | TAX -- X := A
  | TXA -- A := X
  -- Output
  | OUT -- Out := A
  | OUTX -- Out := X
  | OUTI -- Out := M[pc]
  | OUTM -- Out := M[X]
  -- Misc
  | IMM Byte
  deriving Show
