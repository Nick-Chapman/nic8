
module Asm
  ( Byte, Op(..), Asm(..), assemble
  , add, adc, addb, addx, addout, sub, tab, tax, tbx, txa, txb, out, outb, outx, nop, outi
  , spin
  , la, lb, lx, jump, jz, jc, jxu
  , lxa, lxb, lxx
  , variable
  , loadA, loadB, loadX, storeA, storeI, storeAdd, sxa, sxi
  , increment
  , lsr, asr, lsrb, asrb
  ) where

import Control.Monad (ap,liftM)
import Control.Monad.Fix (MonadFix,mfix)
import Op (Byte,Op(..))

add,adc,addb,addx,addout,sub :: Asm () -- arithmetic
tab,tax,tbx,txa,txb :: Asm () -- register transfers
out,outb,outx,nop,spin :: Asm ()
outi :: Byte -> Asm () -- output immediate
la,lb,lx :: Byte -> Asm () -- load immediate into regs
lxa,lxb,lxx :: Asm () -- load *x into reg
jxu :: Asm () -- jumps
jump,jz,jc :: Byte -> Asm () -- jumps
lsr,asr,lsrb,asrb :: Asm ()

variable :: Byte -> Asm Byte -- allocate space for a variable
loadA,loadB,loadX :: Byte -> Asm () -- load vars into regs
storeA :: Byte -> Asm () -- store A into var
storeI :: Byte -> Byte -> Asm () -- store immediate into var
storeAdd :: Byte -> Asm () -- store A+B into var
sxa :: Asm () -- store A into *X
sxi :: Asm () -- store immediate into *X

add = Emit [ADD]
adc = Emit [ADC]
addb = Emit [ADDB]
addx = Emit [ADDX]
addout = Emit [ADDOUT]
sub = Emit [SUB]

lsr = Emit [LSR]
asr = Emit [ASR]
lsrb = Emit [LSR]
asrb = Emit [ASR]

tab = Emit [TAB]
tax = Emit [TAX]
tbx = Emit [TBX]
txa = Emit [TXA]
txb = Emit [TXB]
out = Emit [OUT]
outb = Emit [OUTB]
outx = Emit [OUTX]
outi b = Emit [OUTI, IMM b]
nop = Emit [NOP]

spin = mdo
  Emit [LIX, IMM loc]
  loc <- Here
  Emit [JXU]

la b = Emit [LIA, IMM b]
lb b = Emit [LIB, IMM b]
lx b = Emit [LIX, IMM b]

lxa = Emit [LXA]
lxb = Emit [LXB]
lxx = Emit [LXX]

jxu = Emit [JXU]
jump b = Emit [JIU, IMM b]
jz b = Emit [JIZ, IMM b]
jc b = Emit [JIC, IMM b]

variable val = do
  loc <- Here
  Emit [IMM val]
  pure loc

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
