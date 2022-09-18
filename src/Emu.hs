
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
  SXI -> Cat o o FromProgRom ToDataRam
  JXU -> Cat o o FromX ToPC
  JXZ -> Cat o x FromX ToPC
  JXC -> Cat x o FromX ToPC
  JIU -> Cat o o FromProgRom ToPC
  JIZ -> Cat o x FromProgRom ToPC
  JIC -> Cat x o FromProgRom ToPC
  ADD -> Cat o o FromAlu ToA
  ADDB -> Cat o o FromAlu ToB
  ADDX -> Cat o o FromAlu ToX
  ADDM -> Cat o o FromAlu ToDataRam
  ADDOUT -> Cat o o FromAlu ToOut
  SUB -> Cat o x FromAlu ToA
  SUBB -> Cat o x FromAlu ToB
  SUBX -> Cat o x FromAlu ToX

  LSR -> Cat o o FromShiftedA ToA
  ASR -> Cat o x FromShiftedA ToA
  LSRB -> Cat o o FromShiftedA ToB
  ASRB -> Cat o x FromShiftedA ToB

  OUT -> Cat o o FromA ToOut
  OUTB -> Cat o o FromB ToOut
  OUTX -> Cat o o FromX ToOut
  OUTI -> Cat o o FromProgRom ToOut
  OUTM -> Cat o o FromDataRam ToOut
  TAB -> Cat o o FromA ToB
  TAX -> Cat o o FromA ToX
  TBX -> Cat o o FromB ToX
  TXA -> Cat o o FromX ToA
  TXB -> Cat o o FromX ToB
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


data Source = FromProgRom | FromZero | FromA | FromB | FromX | FromDataRam | FromAlu | FromShiftedA
  deriving (Eq,Show)

encodeSource :: Source -> Byte
encodeSource = \case
  FromProgRom -> 0
  FromZero -> 1 -- drive zero onto bus
  FromA -> 2
  FromB -> 3
  FromX -> 4
  FromDataRam -> 5
  FromAlu -> 6
  FromShiftedA -> 7

decodeSource :: Byte -> Source
decodeSource = \case
  0 -> FromProgRom
  1 -> FromZero
  2 -> FromA
  3 -> FromB
  4 -> FromX
  5 -> FromDataRam
  6 -> FromAlu
  7 -> FromShiftedA
  x -> error (show ("decodeSource",x))


data Dest = ToInstructionRegister | ToDataRam | ToA | ToB | ToX | ToPC | ToOut | ToOutHi
  deriving (Eq,Show)

encodeDest :: Dest -> Byte
encodeDest =  \case
  ToInstructionRegister -> 0
  ToPC -> 1
  ToA -> 2
  ToB -> 3
  ToX -> 4
  ToDataRam -> 5
  ToOut -> 6
  ToOutHi -> 7

decodeDest :: Byte -> Dest
decodeDest = \case
  0 -> ToInstructionRegister
  1 -> ToPC
  2 -> ToA
  3 -> ToB
  4 -> ToX
  5 -> ToDataRam
  6 -> ToOut
  7 -> ToOutHi
  x -> error (show ("decodeDest",x))

----------------------------------------------------------------------
-- Control

data Control = Control
  { provideRom :: Bool
  , provideRam :: Bool
  , provideAlu :: Bool
  , provideShiftedA :: Bool
  , provideA :: Bool
  , provideB :: Bool
  , provideX :: Bool
  , loadIR :: Bool
  , loadPC :: Bool
  , loadA :: Bool
  , loadB :: Bool
  , loadX :: Bool
  , storeMem :: Bool
  , doOut :: Bool
  , doSubtract :: Bool
  , doShiftIn :: Bool
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
    let provideShiftedA = (source == FromShiftedA)
    let provideA = (source == FromA)
    let provideB = (source == FromB)
    let provideX = (source == FromX)
    let loadIR = (dest == ToInstructionRegister)
    let loadPC = (dest == ToPC)
    let loadA = (dest == ToA)
    let loadB = (dest == ToB)
    let loadX = (dest == ToX)
    let storeMem = (dest == ToDataRam)
    let doOut = (dest == ToOut)
    let doSubtract = xbit3
    let doShiftIn = xbit3
    let jumpIfZero = xbit3
    let jumpIfCarry = xbit7
    let unconditionalJump = not xbit3 && not xbit7

    Control {provideRom,provideRam,provideAlu,provideShiftedA
            ,provideA,provideB,provideX
            ,loadIR,loadPC,loadA,loadB,loadX,storeMem
            ,doOut,doSubtract,doShiftIn
            ,jumpIfZero,jumpIfCarry,unconditionalJump}

----------------------------------------------------------------------
-- State

data State = State
  { rom :: Map Byte Byte
  -- , ram :: Map Byte Byte -- h/w will have harvard arch. but some tests here have only unified ram
  , rIR :: Byte
  , rPC :: Byte
  , rA :: Byte
  , rB :: Byte
  , rX :: Byte
  , flagCarry :: Bool
  , flagShift :: Bool
  } deriving Eq

instance Show State where
  show State{rIR,rPC,rA,rB,rX,flagCarry,flagShift} =
    printf "PC=%02X IR=%02X A=%02X B=%02X X=%02X, CARRY=%s, SHIFTED=%s" rPC rIR rA rB rX (show flagCarry) (show flagShift)

initState :: [Op] -> State
initState prog = State
  { rom = initMem prog
  -- , ram = Map.empty
  , rIR = 0
  , rPC = 0
  , rA = 0
  , rB = 0
  , rX = 0
  , flagCarry = False
  , flagShift = False
  }

initMem :: [Op] -> Map Byte Byte
initMem prog = Map.fromList (zip [0..] (map encodeOp prog))

encodeOp :: Op -> Byte
encodeOp = encodeCat . op2cat

data Output = Output Byte

step :: State -> Control -> (State,Maybe Output)
step state control = do
  let State{rom{-,ram-},rIR=_,rPC,rA,rB,rX,flagCarry,flagShift} = state
  let Control{provideRom,provideRam,provideAlu,provideShiftedA
             ,provideA,provideB,provideX
             ,loadA,loadB,loadX,loadIR,loadPC,storeMem
             ,doOut,doSubtract,doShiftIn
             ,jumpIfZero,jumpIfCarry,unconditionalJump} = control
  let aIsZero = (rA == 0)
  let jumpControl = (jumpIfZero && aIsZero) || (jumpIfCarry && flagCarry) || unconditionalJump
  let alu = if doSubtract then (rA - rB) else (rA + rB)
  let carry =
        if doSubtract
        then not (rB > rA)
        else fromIntegral rA + fromIntegral rB >= (256::Int)

  let aShifted =
        (if doShiftIn && flagShift then 128 else 0) + rA `div` 2
  let shiftedOut = rA `mod` 2 == 1
  let dbus =
        case (provideRom,provideRam
             ,provideAlu
             ,provideA,provideB,provideX
             ,provideShiftedA
             ) of
          (True,False,False,False,False,False,False) -> maybe 0 id (Map.lookup rPC rom)
          (False,True,False,False,False,False,False) -> maybe 0 id (Map.lookup rX rom)
          (False,False,True,False,False,False,False) -> alu
          (False,False,False,True,False,False,False) -> rA
          (False,False,False,False,True,False,False) -> rB
          (False,False,False,False,False,True,False) -> rX
          (False,False,False,False,False,False,True) -> aShifted
          (False,False,False,False,False,False,False) -> error "no drivers for data bus"
          p -> error (show ("multiple drivers for data bus",p))

  let s' = State
        { rom = if storeMem then Map.insert rX dbus rom else rom
        -- , ram = if storeMem then Map.insert rX dbus ram else ram
        , rIR = if loadIR then dbus else 0
        , rPC = if loadPC && jumpControl then dbus else if provideRom then rPC + 1 else rPC
        , rA = if loadA then dbus else rA
        , rB = if loadB then dbus else rB
        , rX = if loadX then dbus else rX
        , flagCarry = if provideAlu then carry else flagCarry
        , flagShift = if provideShiftedA then shiftedOut else flagShift
        }
  (s',
   if doOut then Just (Output dbus) else Nothing
    )
