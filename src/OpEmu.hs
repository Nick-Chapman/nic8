
module OpEmu
  ( runIO, runCollectOutput
  , Op(..), Byte,
  ) where

import Data.Bits (testBit,shiftL,shiftR,(.&.))
import Data.Map (Map)
import Data.Word8 (Word8)
import Text.Printf (printf)
import qualified Data.Map as Map

type Byte = Word8

runIO :: [Op] -> IO ()
runIO prog = do
  print prog
  let state0 = initState prog
  loop 0 state0
  where
    loop :: Int -> State -> IO ()
    loop i s = do
      let State{ir} = s
      --printf "%3d : %s : %s\n" i (show s) (show (decodeOp ir))
      let cat = decodeCat ir
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


runCollectOutput :: [Op] -> [Byte]
runCollectOutput prog = do
  let state0 = initState prog
  loop [] state0
  where
    -- TODO: step count limited to prevent buggy infinite tests
    loop :: [Byte] -> State -> [Byte]
    loop acc s = do
      let State{ir} = s
      let cat = decodeCat ir
      let con = cat2control cat
      let (s'Maybe,oMaybe) = step s con
      let acc' = case oMaybe of
            Nothing -> acc
            Just (Output byte) -> byte:acc
      case s'Maybe of
        Just s' -> loop acc' s'
        Nothing -> reverse acc' -- done


----------------------------------------------------------------------
-- Op

data Op
  -- fetch
  = FETCH -- IR := M[pc], pc++
  -- load immediate A/X
  | LIA -- A := M[pc], pc++
  | LIB -- B := M[pc], pc++
  | LIX -- X := M[pc], pc++
  -- load indexed A/X
  | LXA -- A := M[X]
  | LXB -- B := M[X]
  | LXX -- X := M[X]
  -- Store indexed
  | SXA -- M[X] := A
  -- Jumps
  | JIU -- pc := M[pc]
  | JIZ -- pc := zero(A) ? M[pc] : pc+1
  | JXU -- pc := M[X]
  | JXZ -- pc := zero(A) ? M[X] : pc
  | JAU -- pc := A
  -- Arithmetic
  | ADD -- A := A+B
  | SUB -- A := A-B
  | ADX -- X := A+B
  | SUX -- X := A-B
  | ADDM -- M[X] := A+B
  -- Misc
  | OUT -- A -> Out
  | TAB -- B := A
  | TAX -- X := A
  | HLT
  | IMM Byte
  deriving Show

encodeOp :: Op -> Byte
encodeOp = encodeCat . op2cat


----------------------------------------------------------------------
-- Cat

data Cat = Cat -- Control atributes
  { xbit7 :: Bool
  , xbit6 :: Bool -- if-zero bit for jump instruction; sub-bit for alu
  , writer :: WriterD -- bit 5,4
  , reader :: ReaderD -- bit 3,2,1
  , indexed :: Bool -- bit 0 (address bus driven from X otherwise PC++)
  }
  | Lit Byte
  deriving Show

op2cat :: Op -> Cat
op2cat = \case
  FETCH -> Cat o o FromMem ToI o
  LIA -> Cat o o FromMem ToA o
  LIB -> Cat o o FromMem ToB o
  LIX -> Cat o o FromMem ToM o
  LXA -> Cat o o FromMem ToA x
  LXB -> Cat o o FromMem ToB x
  LXX -> Cat o o FromMem ToM x
  SXA -> Cat o o FromAcc Store x
  JIU -> Cat o x FromMem ToP  o
  JIZ -> Cat o o FromMem ToP  o
  JXU -> Cat o x FromMem ToP  x
  JXZ -> Cat o o FromMem ToP  x
  JAU -> Cat o x FromAcc ToP  x
  ADD -> Cat o o FromAlu ToA x
  ADDM -> Cat o o FromAlu Store x
  SUB -> Cat o x FromAlu ToA x
  ADX -> Cat o o FromAlu ToM x
  SUX -> Cat o x FromAlu ToM x
  OUT -> Cat o o FromAcc Out x
  TAB -> Cat o o FromAcc ToB x
  TAX -> Cat o o FromAcc ToM x
  HLT -> Cat o o FromMem Halt x
  IMM b -> Lit b
  where
    o = False
    x = True

encodeCat :: Cat -> Byte
encodeCat = \case
  Cat{xbit7,xbit6,writer,reader,indexed} ->
    0
    + (if xbit7 then 1 else 0) `shiftL` 7
    + (if xbit6 then 1 else 0) `shiftL` 6
    + encodeWriter writer `shiftL` 4
    + encodeReader reader `shiftL` 1
    + (if indexed then 1 else 0)
  Lit b ->
    b

decodeCat :: Byte -> Cat
decodeCat b = do
  let xbit7 = b `testBit` 7
  let xbit6 = b `testBit` 6
  let writer = decodeWriter ((b `shiftR` 4) .&. 3)
  let reader = decodeReader ((b `shiftR` 1) .&. 7)
  let indexed = b `testBit` 0
  Cat {xbit7,xbit6,writer,reader,indexed}


data WriterD = FromAcc | FromMem | FromAlu | FromPC
  deriving Show

encodeWriter :: WriterD -> Byte
encodeWriter = \case
  FromMem -> 0
  FromAcc -> 1
  FromAlu -> 2
  FromPC -> 3

decodeWriter :: Byte -> WriterD
decodeWriter = \case
  0 -> FromMem
  1 -> FromAcc
  2 -> FromAlu
  3 -> FromPC
  x -> error (show ("decodeWriter",x))


data ReaderD = ToI | ToP | ToA | ToB | ToM | Store | Out | Halt
  deriving (Eq,Show)

encodeReader :: ReaderD -> Byte
encodeReader =  \case
  ToI -> 0
  ToP -> 1
  ToA -> 2
  ToB -> 3
  ToM -> 4
  Store -> 5
  Out -> 6
  Halt -> 7

decodeReader :: Byte -> ReaderD
decodeReader = \case
  0 -> ToI
  1 -> ToP
  2 -> ToA
  3 -> ToB
  4 -> ToM
  5 -> Store
  6 -> Out
  7 -> Halt
  x -> error (show ("decodeReader",x))

----------------------------------------------------------------------
-- Control

data Control = Control
  { writeAbus :: WriterA
  , writeDbus :: WriterD
  , loadIR :: Bool
  , loadPC :: Bool
  , loadAcc :: Bool
  , loadB :: Bool
  , loadMar :: Bool
  , storeMem :: Bool
  , doOut :: Bool
  , halt :: Bool
  , incPC :: Bool
  , doSubtract :: Bool
  , unconditionalJump :: Bool
  } deriving Show

data WriterA = WA_PC | WA_Mar deriving Show

cat2control :: Cat -> Control
cat2control = \case
  Lit{} -> error "unexpected Cat/Lit"
  Cat{xbit6,reader,writer,indexed} -> do
    let writeAbus = if indexed then WA_Mar else WA_PC
    let writeDbus = writer
    let loadIR = (reader == ToI)
    let loadPC = (reader == ToP)
    let loadAcc = (reader == ToA)
    let loadB = (reader == ToB)
    let loadMar = (reader == ToM)
    let storeMem = (reader == Store)
    let doOut = (reader == Out)
    let halt = (reader == Halt)
    let incPC = not indexed
    let doSubtract = xbit6
    let unconditionalJump = xbit6
    Control {writeAbus,writeDbus
            ,loadIR,loadPC,loadAcc,loadB,loadMar,storeMem
            ,doOut,halt,incPC,doSubtract,unconditionalJump}

----------------------------------------------------------------------
-- State

data State = State
  { mem :: Map Byte Byte
  , ir :: Byte
  , pc :: Byte
  , acc :: Byte
  , b :: Byte
  , mar :: Byte
  }

instance Show State where
  show State{ir,pc,acc,b,mar} =
    printf "PC=%02X IR=%02X A=%02X B=%02X MAR=%02X" pc ir acc b mar

initState :: [Op] -> State
initState prog = State
  { mem = initMem prog
  , ir = encodeOp FETCH
  , pc = 0
  , acc = 0
  , b = 0
  , mar = 0
  }

initMem :: [Op] -> Map Byte Byte
initMem prog = Map.fromList (zip [0..] (map encodeOp prog))

data Output = Output Byte

step :: State -> Control -> (Maybe State,Maybe Output)
step state control = do
  let State{mem,ir=_,pc,acc,b,mar} = state
  let Control{writeAbus,writeDbus
             ,loadAcc,loadB,loadMar,loadIR,loadPC,storeMem
             ,doOut,halt,incPC,doSubtract,unconditionalJump} = control
  let aIsZero = (acc == 0)
  let jumpControl = unconditionalJump || aIsZero
  let abus = case writeAbus of
        WA_PC -> pc
        WA_Mar -> mar
  let alu =
        if doSubtract then (acc - b) else (acc + b) -- `mod` 256 -- sub!
  let dbus = case writeDbus of
        FromAcc -> acc
        FromMem -> maybe 0 id (Map.lookup abus mem)
        FromAlu -> alu
        FromPC -> pc
  let s' = State
        { mem = if storeMem then Map.insert abus dbus mem else mem
        , ir = if loadIR then dbus else 0
        , pc = if loadPC && jumpControl then dbus else if incPC then incByte pc else pc
        , acc = if loadAcc then dbus else acc
        , b = if loadB then dbus else b
        , mar = if loadMar then dbus else mar
        }
  (if halt then Nothing else Just s',
   if doOut then Just (Output dbus) else Nothing
    )

incByte :: Byte -> Byte
incByte b = if b == 255 then 0 else b + 1
