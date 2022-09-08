
module Emu
  ( runCollectOutput
  , encodeOp
  , Cycles(..), OutOfGas(..),
  ) where

import Data.Bits (testBit,shiftL,shiftR,(.&.))
import Data.Map (Map)
import Op (Op(..),Byte)
import Text.Printf (printf)
import qualified Data.Map as Map

runCollectOutput :: Cycles -> [Op] -> Either (OutOfGas,[Byte]) (Cycles,[Byte])
runCollectOutput max prog = do
  let state0 = initState prog
  loop (Cycles 0) [] state0 state0
  where
    loop :: Cycles -> [Byte] -> State -> State -> Either (OutOfGas,[Byte]) (Cycles,[Byte])
    loop cycles acc sMinus1 s0 = if cycles > max then Left (OutOfGas, reverse acc) else do
      --print (cycles,s)
      let State{rIR} = s0
      let cat = decodeCat rIR
      let con = cat2control cat
      let (s1,oMaybe) = step s0 con
      let acc' = case oMaybe of
            Nothing -> acc
            Just (Output byte) -> byte:acc
      if s1 == sMinus1
        then Right (cycles - 2, reverse acc')
        else loop (cycles+1) acc' s0 s1

data OutOfGas = OutOfGas deriving Show
newtype Cycles = Cycles Int deriving (Eq,Ord,Num,Show)

----------------------------------------------------------------------
-- Cat

data Cat = Cat -- Control atributes
  { xbit7 :: Bool
  , xbit3 :: Bool -- if-zero bit for jump instruction; sub-bit for alu
  , source :: Source -- bit 2,1,0
  , dest :: Dest -- bit 6,5,4
  }
  | Lit Byte
  deriving Show

op2cat :: Op -> Cat
op2cat = \case
  NOP -> Cat o o FromProgRom ToInstructionRegister
  LIA -> Cat o o FromProgRom ToA
  LIB -> Cat o o FromProgRom ToB
  LIX -> Cat o o FromProgRom ToX
  LXA -> Cat o o FromDataRam ToA
  LXB -> Cat o o FromDataRam ToB
  LXX -> Cat o o FromDataRam ToX
  SXA -> Cat o o FromA ToDataRam
  JXU -> Cat o o FromX ToP
  JXZ -> Cat o x FromX ToP
  JXC -> Cat x o FromX ToP
  JIU -> Cat o o FromProgRom ToP
  ADD -> Cat o o FromAlu ToA
  ADDB -> Cat o o FromAlu ToB
  ADDX -> Cat o o FromAlu ToX
  ADDM -> Cat o o FromAlu ToDataRam
  ADDOUT -> Cat o o FromAlu ToOut
  SUB -> Cat o x FromAlu ToA
  SUBB -> Cat o x FromAlu ToB
  SUBX -> Cat o x FromAlu ToX
  OUT -> Cat o o FromA ToOut
  OUTX -> Cat o o FromX ToOut
  OUTI -> Cat o o FromProgRom ToOut
  OUTM -> Cat o o FromDataRam ToOut
  TAB -> Cat o o FromA ToB
  TAX -> Cat o o FromA ToX
  TXA -> Cat o o FromX ToA
  IMM b -> Lit b
  where
    o = False
    x = True

encodeCat :: Cat -> Byte
encodeCat = \case
  Cat{xbit7,xbit3,source,dest} ->
    0
    + (if xbit7 then 1 else 0) `shiftL` 7
    + encodeDest dest `shiftL` 4
    + (if xbit3 then 1 else 0) `shiftL` 3
    + encodeSource source
  Lit b ->
    b

decodeCat :: Byte -> Cat
decodeCat b = do
  let xbit7 = b `testBit` 7
  let dest = decodeDest ((b `shiftR` 4) .&. 7)
  let xbit3 = b `testBit` 3
  let source = decodeSource (b .&. 7)
  Cat {xbit7,xbit3,source,dest}


data Source = FromProgRom | FromDataRam | FromA | FromB | FromX | FromAlu | FromNowhere
  deriving (Eq,Show)

encodeSource :: Source -> Byte
encodeSource = \case
  FromProgRom -> 0
  FromDataRam -> 1
  FromA -> 2
  FromB -> 3
  FromX -> 4
  FromAlu -> 5
  FromNowhere -> 6

decodeSource :: Byte -> Source
decodeSource = \case
  0 -> FromProgRom
  1 -> FromDataRam
  2 -> FromA
  3 -> FromB
  4 -> FromX
  5 -> FromAlu
  6 -> FromNowhere
  7 -> FromNowhere
  x -> error (show ("decodeSource",x))


data Dest = ToInstructionRegister | ToDataRam | ToA | ToB | ToX | ToP | ToOut | ToNowhere
  deriving (Eq,Show)

encodeDest :: Dest -> Byte
encodeDest =  \case
  ToInstructionRegister -> 0
  ToDataRam -> 1
  ToA -> 2
  ToB -> 3
  ToX -> 4
  ToP -> 5
  ToOut -> 6
  ToNowhere -> 7
  -- destination 7 available for future expansions! - OutHi ?

decodeDest :: Byte -> Dest
decodeDest = \case
  0 -> ToInstructionRegister
  1 -> ToDataRam
  2 -> ToA
  3 -> ToB
  4 -> ToX
  5 -> ToP
  6 -> ToOut
  7 -> ToNowhere
  x -> error (show ("decodeDest",x))

----------------------------------------------------------------------
-- Control

data Control = Control
  { provideRom :: Bool
  , provideRam :: Bool
  , provideAlu :: Bool
  , provideA :: Bool
  , provideX :: Bool
  , loadIR :: Bool
  , loadPC :: Bool
  , loadA :: Bool
  , loadB :: Bool
  , loadX :: Bool
  , storeMem :: Bool
  , doOut :: Bool
  , doSubtract :: Bool
  , jumpIfZero :: Bool
  , jumpIfCarry :: Bool
  , unconditionalJump :: Bool
  } deriving Show

cat2control :: Cat -> Control
cat2control = \case
  Lit{} -> error "unexpected Cat/Lit"
  Cat{xbit7,xbit3,dest,source} -> do
    let provideRom = (source == FromProgRom)
    let provideRam = (source == FromDataRam)
    let provideAlu = (source == FromAlu)
    let provideA = (source == FromA)
    let provideX = (source == FromX)
    let loadIR = (dest == ToInstructionRegister)
    let loadPC = (dest == ToP)
    let loadA = (dest == ToA)
    let loadB = (dest == ToB)
    let loadX = (dest == ToX)
    let storeMem = (dest == ToDataRam)
    let doOut = (dest == ToOut)
    let doSubtract = xbit3
    let jumpIfZero = xbit3
    let jumpIfCarry = xbit7
    let unconditionalJump = not xbit3 && not xbit7

    Control {provideRom,provideRam,provideAlu,provideA,provideX
            ,loadIR,loadPC,loadA,loadB,loadX,storeMem
            ,doOut,doSubtract
            ,jumpIfZero,jumpIfCarry,unconditionalJump}

----------------------------------------------------------------------
-- State

data State = State
  { mem :: Map Byte Byte
  , rIR :: Byte
  , rPC :: Byte
  , rA :: Byte
  , rB :: Byte
  , rX :: Byte
  , flagCarry :: Bool
  } deriving Eq

instance Show State where
  show State{rIR,rPC,rA,rB,rX,flagCarry} =
    printf "PC=%02X IR=%02X A=%02X B=%02X X=%02X, CARRY=%s" rPC rIR rA rB rX (show flagCarry)

initState :: [Op] -> State
initState prog = State
  { mem = initMem prog
  , rIR = 0
  , rPC = 0
  , rA = 0
  , rB = 0
  , rX = 0
  , flagCarry = False
  }

initMem :: [Op] -> Map Byte Byte
initMem prog = Map.fromList (zip [0..] (map encodeOp prog))

encodeOp :: Op -> Byte
encodeOp = encodeCat . op2cat

data Output = Output Byte

step :: State -> Control -> (State,Maybe Output)
step state control = do
  let State{mem,rIR=_,rPC,rA,rB,rX,flagCarry} = state
  let Control{provideRom,provideRam,provideAlu,provideA,provideX
             ,loadA,loadB,loadX,loadIR,loadPC,storeMem
             ,doOut,doSubtract
             ,jumpIfZero,jumpIfCarry,unconditionalJump} = control
  let aIsZero = (rA == 0)
  let jumpControl = (jumpIfZero && aIsZero) || (jumpIfCarry && flagCarry) || unconditionalJump
  let alu = if doSubtract then (rA - rB) else (rA + rB)
  let carry =
        if doSubtract
        then not (rB > rA)
        else fromIntegral rA + fromIntegral rB >= (256::Int)

  let dbus =
        case (provideRom,provideRam,provideAlu,provideA,provideX) of
          (True,False,False,False,False) -> maybe 0 id (Map.lookup rPC mem)
          (False,True,False,False,False) -> maybe 0 id (Map.lookup rX mem)
          (False,False,True,False,False) -> alu
          (False,False,False,True,False) -> rA
          (False,False,False,False,True) -> rX
          (False,False,False,False,False) -> error "no drivers for data bus"
          p -> error (show ("multiple drivers for data bus",p))

  let s' = State
        { mem = if storeMem then Map.insert rX dbus mem else mem
        , rIR = if loadIR then dbus else 0
        , rPC = if loadPC && jumpControl then dbus else if provideRom then rPC + 1 else rPC
        , rA = if loadA then dbus else rA
        , rB = if loadB then dbus else rB
        , rX = if loadX then dbus else rX
        , flagCarry = if provideAlu then carry else flagCarry
        }
  (s',
   if doOut then Just (Output dbus) else Nothing
    )
