
module Emu
  ( runIO, runCollectOutput, encodeOp
  , Cycles(..), OutOfGas(..)
  ) where

import Data.Bits (testBit,shiftL,shiftR,(.&.))
import Data.Map (Map)
import Op (Op(..),Byte)
import Text.Printf (printf)
import qualified Data.Map as Map

runIO :: [Op] -> IO ()
runIO prog = do
  --print prog
  let state0 = initState prog
  loop 0 state0
  where
    loop :: Int -> State -> IO ()
    loop i s = do
      let State{rIR} = s
      --printf "%3d : %s : %s\n" i (show s) (show (decodeOp rIR))
      let cat = decodeCat rIR
      --print cat
      let con = cat2control cat
      --print con
      let (s'Maybe,oMaybe) = step s con
      case oMaybe of
        Nothing -> pure ()
        Just (Output byte) -> print ("output",byte)
      case s'Maybe of
        Nothing -> do
          --print (mem s)
          pure () --done
        Just s' -> loop (i+1) s'

runCollectOutput :: Cycles -> [Op] -> Either (OutOfGas,[Byte]) (Cycles,[Byte])
runCollectOutput max prog = do
  let state0 = initState prog
  loop (Cycles 0) [] state0
  where
    loop :: Cycles -> [Byte] -> State -> Either (OutOfGas,[Byte]) (Cycles,[Byte])
    loop cycles acc s = if cycles > max then Left (OutOfGas, reverse acc) else do
      let State{rIR} = s
      let cat = decodeCat rIR
      let con = cat2control cat
      let (s'Maybe,oMaybe) = step s con
      let acc' = case oMaybe of
            Nothing -> acc
            Just (Output byte) -> byte:acc
      case s'Maybe of
        Nothing -> Right (cycles, reverse acc') -- done
        Just s' -> loop (cycles+1) acc' s'


data OutOfGas = OutOfGas deriving Show
newtype Cycles = Cycles Int deriving (Eq,Ord,Num,Show)


----------------------------------------------------------------------
-- Cat

data Cat = Cat -- Control atributes
  { xbit7 :: Bool
  , xbit6 :: Bool -- if-zero bit for jump instruction; sub-bit for alu
  , source :: Source -- bit 5,4
  , dest :: Dest -- bit 3,2,1
  , indexed :: Bool -- bit 0 (address bus driven from X otherwise PC++)
  }
  | Lit Byte
  deriving Show

op2cat :: Op -> Cat
op2cat = \case
  NOP -> Cat o o FromMem ToI o
  LIA -> Cat o o FromMem ToA o
  LIB -> Cat o o FromMem ToB o
  LIX -> Cat o o FromMem ToX o
  LXA -> Cat o o FromMem ToA x
  LXB -> Cat o o FromMem ToB x
  LXX -> Cat o o FromMem ToX x
  SXA -> Cat o o FromA ToMem x
  JXU -> Cat x x FromX ToP x
  JXZ -> Cat o x FromX ToP x
  JXC -> Cat x o FromX ToP x
  ADD -> Cat o o FromAlu ToA x
  ADDB -> Cat o o FromAlu ToB x
  ADDX -> Cat o o FromAlu ToX x
  ADDM -> Cat o o FromAlu ToMem x
  ADDOUT -> Cat o o FromAlu ToOut x
  SUB -> Cat o x FromAlu ToA x
  SUBB -> Cat o x FromAlu ToB x
  SUBX -> Cat o x FromAlu ToX x
  OUT -> Cat o o FromA ToOut x
  OUTX -> Cat o o FromX ToOut x
  OUTI -> Cat o o FromMem ToOut o
  OUTM -> Cat o o FromMem ToOut x
  TAB -> Cat o o FromA ToB x
  TAX -> Cat o o FromA ToX x
  TXA -> Cat o o FromX ToA x
  HLT -> Cat x x FromX ToHalt x
  -- HLT -> Cat o o FromX ToX x -- encode HLT as X->X
  -- further unwanted source/dest pairs which can encode something else: FromMem/ToMem, FromA/ToA
  IMM b -> Lit b
  where
    o = False
    x = True

encodeCat :: Cat -> Byte
encodeCat = \case
  Cat{xbit7,xbit6,source,dest,indexed} ->
    0
    + (if xbit7 then 1 else 0) `shiftL` 7
    + (if xbit6 then 1 else 0) `shiftL` 6
    + encodeSource source `shiftL` 4
    + encodeDest dest `shiftL` 1
    + (if indexed then 1 else 0)
  Lit b ->
    b

decodeCat :: Byte -> Cat
decodeCat b = do
  let xbit7 = b `testBit` 7
  let xbit6 = b `testBit` 6
  let source = decodeSource ((b `shiftR` 4) .&. 3)
  let dest = decodeDest ((b `shiftR` 1) .&. 7)
  let indexed = b `testBit` 0
  Cat {xbit7,xbit6,source,dest,indexed}


data Source = FromMem | FromAlu | FromA | FromX
  deriving (Eq,Show)

encodeSource :: Source -> Byte
encodeSource = \case
  FromMem -> 0
  FromAlu -> 1
  FromA -> 2
  FromX -> 3

decodeSource :: Byte -> Source
decodeSource = \case
  0 -> FromMem
  1 -> FromAlu
  2 -> FromA
  3 -> FromX
  x -> error (show ("decodeSource",x))


data Dest = ToI | ToP | ToA | ToB | ToX | ToMem | ToOut | ToHalt
  deriving (Eq,Show)

encodeDest :: Dest -> Byte
encodeDest =  \case
  ToI -> 0
  ToP -> 1
  ToA -> 2
  ToX -> 3
  ToB -> 4
  ToMem -> 5
  ToOut -> 6
  ToHalt -> 7
  -- destination 7 available for future expansions! - not any more

decodeDest :: Byte -> Dest
decodeDest = \case
  0 -> ToI
  1 -> ToP
  2 -> ToA
  3 -> ToX
  4 -> ToB
  5 -> ToMem
  6 -> ToOut
  7 -> ToHalt
  x -> error (show ("decodeDest",x))

----------------------------------------------------------------------
-- Control

data Control = Control
  { provideMem
  , provideAlu
  , provideA
  , provideX
  , loadIR :: Bool
  , loadPC :: Bool
  , loadA :: Bool
  , loadB :: Bool
  , loadX :: Bool
  , storeMem :: Bool
  , doOut :: Bool
  , halt :: Bool
  , immediate :: Bool
  , doSubtract :: Bool
  , jumpIfZero :: Bool
  , jumpIfCarry :: Bool
  , unconditionalJump :: Bool
  } deriving Show

cat2control :: Cat -> Control
cat2control = \case
  Lit{} -> error "unexpected Cat/Lit"
  Cat{xbit7,xbit6,dest,source,indexed} -> do
    let provideMem = (source == FromMem)
    let provideAlu = (source == FromAlu)
    let provideA = (source == FromA)
    let provideX = (source == FromX)
    let loadIR = (dest == ToI)
    let loadPC = (dest == ToP)
    let loadA = (dest == ToA)
    let loadB = (dest == ToB)
    let loadX = (dest == ToX)
    let storeMem = (dest == ToMem)
    let doOut = (dest == ToOut)
    let halt = (dest == ToHalt)
    --let halt = (provideX && loadX)
    let immediate = not indexed
    let doSubtract = xbit6
    let jumpIfZero = xbit6
    let jumpIfCarry = xbit7
    let unconditionalJump = xbit6 && xbit7
    Control {provideMem,provideAlu,provideA,provideX
            ,loadIR,loadPC,loadA,loadB,loadX,storeMem
            ,doOut,halt,immediate,doSubtract
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
  }

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

step :: State -> Control -> (Maybe State,Maybe Output)
step state control = do
  let State{mem,rIR=_,rPC,rA,rB,rX,flagCarry} = state
  let Control{provideMem,provideAlu,provideA,provideX
             ,loadA,loadB,loadX,loadIR,loadPC,storeMem
             ,doOut,halt,immediate,doSubtract
             ,jumpIfZero,jumpIfCarry,unconditionalJump} = control
  let aIsZero = (rA == 0)
  let jumpControl = (jumpIfZero && aIsZero) || (jumpIfCarry && flagCarry) || unconditionalJump
  let abus = if immediate then rPC else rX
  let alu = if doSubtract then (rA - rB) else (rA + rB)
  let carry =
        if doSubtract
        then not (rB > rA)
        else fromIntegral rA + fromIntegral rB >= (256::Int)
  let dbus =
        case (provideMem,provideAlu,provideA,provideX) of
          (True,False,False,False) -> maybe 0 id (Map.lookup abus mem)
          (False,True,False,False) -> alu
          (False,False,True,False) -> rA
          (False,False,False,True) -> rX
          (False,False,False,False) -> error "no drivers for data bus"
          p -> error (show ("multiple drivers for data bus",p))
  let s' = State
        { mem = if storeMem then Map.insert abus dbus mem else mem
        , rIR = if loadIR then dbus else 0
        , rPC = if loadPC && jumpControl then abus else if immediate then rPC + 1 else rPC
        , rA = if loadA then dbus else rA
        , rB = if loadB then dbus else rB
        , rX = if loadX then dbus else rX
        , flagCarry = if provideAlu then carry else flagCarry
        }
  (if halt then Nothing else Just s',
   if doOut then Just (Output dbus) else Nothing
    )
