
module Asm
  ( Byte, Op(..), Asm(..), assemble
  , add, tadd, adc, addb, addx, addout, sub, tsub, tab, tax, tba, tbx, txa, txb, out, outb, outx, nop, outi, outm
  , lza, la, lb, lx, jump, jz, jc, js, jxu, jxc
  , lxa, lxb, lxx
  , loadA, loadB, loadX, storeA, storeI, storeAdd, sxa, sxi, sxz
  , increment
  , lsr, tlsr, asr, lsrb, asrb
  , spin
  ) where

import Control.Monad (ap,liftM)
import Control.Monad.Fix (MonadFix,mfix)
import Op (Byte,Op(..))

add,tadd,adc,addb,addx,addout,sub,tsub :: Asm () -- arithmetic
tab,tax,tba,tbx,txa,txb :: Asm () -- register transfers
out,outb,outx,nop :: Asm ()
outi,outm :: Byte -> Asm () -- output immediate
lza :: Asm () -- zero registers
la,lb,lx :: Byte -> Asm () -- load immediate into regs
lxa,lxb,lxx :: Asm () -- load *x into reg
jxu,jxc :: Asm () -- jumps
jump,jz,jc,js :: Byte -> Asm () -- jumps
lsr,tlsr,asr,lsrb,asrb :: Asm ()
spin :: Asm () -- almost like halt!

loadA,loadB,loadX :: Byte -> Asm () -- load vars into regs
storeA :: Byte -> Asm () -- store A into var
storeI :: Byte -> Byte -> Asm () -- store immediate into var
storeAdd :: Byte -> Asm () -- store A+B into var
sxa :: Asm () -- store A into *X
sxi :: Asm () -- store immediate into *X
sxz :: Asm () -- store zero into *X

add = Emit [ADD]
tadd = Emit [TADD]
adc = Emit [ADC]
addb = Emit [ADDB]
addx = Emit [ADDX]
addout = Emit [ADDOUT]
sub = Emit [SUB]
tsub = Emit [TSUB]

lsr = Emit [LSR]
tlsr = Emit [TLSR]
asr = Emit [ASR]
lsrb = Emit [LSRB]
asrb = Emit [ASR]

tab = Emit [TAB]
tax = Emit [TAX]
tba = Emit [TBA]
tbx = Emit [TBX]
txa = Emit [TXA]
txb = Emit [TXB]
out = Emit [OUT]
outb = Emit [OUTB]
outx = Emit [OUTX]
outi b = Emit [OUTI, IMM b]
outm loc = Emit [LIX, IMM loc, OUTM]
nop = Emit [NOP]

lza = Emit [LZA]
la b = Emit [LIA, IMM b]
lb b = Emit [LIB, IMM b]
lx b = Emit [LIX, IMM b]

lxa = Emit [LXA]
lxb = Emit [LXB]
lxx = Emit [LXX]

jxu = Emit [JXU]
jxc = Emit [JXC]
jump b = Emit [JIU, IMM b]
jz b = Emit [JIZ, IMM b]
jc b = Emit [JIC, IMM b]
js b = Emit [JIS, IMM b]

loadA loc = Emit [LIX, IMM loc, LXA]
loadB loc = Emit [LIX, IMM loc, LXB]
loadX loc = Emit [LIX, IMM loc, LXX]

storeA loc = Emit [LIX, IMM loc, SXA]
storeI b loc = Emit [LIX, IMM loc, SXI, IMM b]
storeAdd loc = Emit [LIX, IMM loc, ADDM]

increment :: Byte -> Byte -> Asm ()
increment var n = do
  loadA var
  lb n
  add
  storeA var

sxa = Emit [SXA]
sxi = Emit [SXI]
sxz = Emit [SXZ]

spin = mdo
  lx loc
  loc <- Here
  jxu

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = return; (<*>) = ap
instance Monad Asm where return = Ret; (>>=) = Bind
instance MonadFix Asm where mfix = Mfix

data Asm a where
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Emit :: [Op] -> Asm ()
  Here :: Asm Byte
  Mfix :: (a -> Asm a) -> Asm a

assemble :: Asm () -> [Op]
assemble = reverse . snd . loop []
  where
    loop :: [Op] -> Asm a -> (a,[Op])
    loop acc = \case
      Ret a -> (a,acc)
      Bind m f -> do
        let (a,acc1) = loop acc m
        loop acc1 (f a)
      Emit ops -> ((), reverse ops ++ acc)
      Here -> (fromIntegral $ length acc, acc)
      Mfix f -> do
        let x@(a,_) = loop acc (f a)
        x
