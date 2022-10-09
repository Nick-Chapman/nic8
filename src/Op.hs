
module Op (Op(..), Byte, allOps) where

import Data.Word8 (Word8)

type Byte = Word8

data Op
  -- nop/fetch -- must be encoded as zero
  = NOP -- IR := M[pc], pc++
  -- laod zero
  | LZA -- A := 0
  | LZX -- X := 0
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
  | SXI -- M[X] := ROM[pc], pc++
  | SXZ -- M[X] := 0
  -- Jumps
  | JXU -- pc := X
  | JXZ -- pc := zero(A) ? X : pc+1
  | JXC -- pc := carryFlag ? X : pc+1
  | JXS -- pc := shiftFlag ? X : pc+1
  | JIU -- pc := M[pc]
  | JIZ -- pc := zero(A) ? M[pc] : pc+1
  | JIC -- pc := carryFlag ? M[pc] : pc+1
  | JIS -- pc := shiftFlag ? M[pc] : pc+1
  -- Arithmetic (also sets overflow flag)
  | ADD -- {carryFlag,A} := A+B
  | TADD -- carryFlag := A+B>=256
  | ADC -- {carryFlag,A} := A+B+cin
  | ADDB -- B := A+B
  | ADDX -- X := A+B
  | ADDM -- M[X] := A+B
  | ADCM -- M[X] := A+B+cin
  | ADDOUT -- OUT := A+B
  | SUB -- A := A-B
  | TSUB
  | SBC -- A := A-B-cin
  | SUBB -- B := A-B
  | SUBX -- X := A-B
  -- shifting
  | LSR -- A := A>>1; shifFlag=A[0]
  | LSRM -- M := A>>1; shifFlag=A[0]
  | TLSR -- shifFlag=A[0]
  | ASR -- A := (shifFlag?128:0) | A>>1; shiftFlag=A[0]
  | LSRB -- B := A>>1; shifFlag=A[0]
  | ASRB -- B := (shifFlag?128:0) | A>>1; shiftFlag=A[0]
  -- more SUBS possible too
  -- Register transfers
  | TAB -- B := A
  | TAX -- X := A
  | TBA -- A := B
  | TBX -- X := B
  | TXA -- A := X
  | TXB -- B := X
  -- Output
  | OUT -- Out := A
  | OUTB -- Out := B
  | OUTX -- Out := X
  | OUTI -- Out := M[pc]
  | OUTM -- Out := M[X]
  -- Misc
  | IMM Byte
  deriving Show


allOps :: [Op]
allOps =
  [ NOP
  , LZA
  , LZX
  , LIA
  , LIB
  , LIX
  , LXA
  , LXB
  , LXX
  , SXA
  , SXI
  , SXZ
  , JXU
  , JXZ
  , JXC
  , JIU
  , ADD
  , TADD
  , ADC
  , ADDB
  , ADDX
  , ADDM
  , ADCM
  , ADDOUT
  , SUB
  , TSUB
  , SBC
  , SUBB
  , SUBX
  , ASR
  , LSR
  , LSRM
  , TLSR
  , ASRB
  , LSRB
  , TAB
  , TAX
  , TBA
  , TBX
  , TXA
  , TXB
  , OUT
  , OUTB
  , OUTX
  , OUTI
  , OUTM
  ]
