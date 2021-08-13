
module Asm
  ( Op(..), Asm(..), assemble
  , add, addx, sub, tab, txa, out, halt
  , la, lb, lx, jump, jz, jv
  , variable
  , loadA, loadB, loadX, storeA, storeAdd
  ) where

import Control.Monad (ap,liftM)
import Control.Monad.Fix (MonadFix,mfix)
import Op (Byte,Op(..))

add,addx,sub :: Asm () -- arithmetic
tab,txa :: Asm () -- register transfers
out,halt :: Asm ()
la,lb,lx :: Byte -> Asm () -- load immediate into regs
jump,jz,jv :: Byte -> Asm () -- jumps

variable :: Byte -> Asm Byte -- allocate space for a variable
loadA,loadB,loadX :: Byte -> Asm () -- load vars into regs
storeA :: Byte -> Asm () -- store A into var
storeAdd :: Byte -> Asm () -- store A+B into var

add = Emit [ADD]
addx = Emit [ADDX]
sub = Emit [SUB]

tab = Emit [TAB]
txa = Emit [TXA]
out = Emit [OUT]
halt = Emit [HLT]

la b = Emit [LIA, IMM b]
lb b = Emit [LIB, IMM b]
lx b = Emit [LIX, IMM b]

jump b = Emit [JIU, IMM b]
jz b = Emit [JIZ, IMM b]
jv b = Emit [JIV, IMM b]

variable val = do
  loc <- Here
  Emit [IMM val]
  pure loc

loadA loc = Emit [LIX, IMM loc, LXA]
loadB loc = Emit [LIX, IMM loc, LXB]
loadX loc = Emit [LIX, IMM loc, LXX]

storeA loc = Emit [LIX, IMM loc, SXA]
storeAdd loc = Emit [LIX, IMM loc, ADDM]


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
