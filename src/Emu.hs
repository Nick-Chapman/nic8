
module Emu
  ( runCollectOutput
  , encodeOp
  , Cycles(..), OutOfGas(..)
  , sim, run
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
      let (s1,oMaybe) = stepWithControl s0 con
      let acc' = case oMaybe of
            Nothing -> acc
            Just (Output byte) -> byte:acc
      if s1 == sMinus1
        then Right (cycles - 2, reverse acc')
        else loop (cycles+1) acc' s0 s1

data OutOfGas = OutOfGas deriving Show
newtype Cycles = Cycles Int deriving (Eq,Ord,Num,Show)

----------------------------------------------------------------------
-- simulation (see state updates; as in verilog)

sim :: Int -> [Op] -> [String]
sim n prog = do
  let ups = take (n+1) $ makeUpdates $ Emu.simulate prog
  banner ++ map show ups

banner :: [String]
banner = [bar," IR | AR BR XR | PC | QR [dec]",bar]
  where bar = "------------------------------"

makeUpdates :: [State] -> [Update]
makeUpdates = \case
  [] -> []
  [s1] -> [state2update s1]
  states@(s1:states') ->
    state2update s1 : [ makeUpdate s s' | (s,s') <- zip states states' ]


run :: Int -> [Op] -> [Byte]
run n prog = changes [ rQ | State{rQ} <- take n $ drop 1 $ Emu.simulate prog ]
  where
    changes :: Eq a => [a] -> [a]
    changes = \case
      [] -> []
      [x] -> [x]
      x:xs@(x2:_) -> if x==x2 then changes xs else x : changes xs


simulate :: [Op] -> [State]
simulate prog = loop (initState prog)
  where
    loop :: State -> [State]
    loop s = s : loop (step s)

    step :: State -> State
    step s0 = do
      let State{rIR} = s0
      let cat = decodeCat rIR
      let con = cat2control cat
      let (s1,_oMaybe) = stepWithControl s0 con
      s1

----------------------------------------------------------------------
-- Updates

data Update = Update
  { uPC :: Maybe Byte
  , uA :: Maybe Byte
  , uB :: Maybe Byte
  , uX :: Maybe Byte
  , uIR :: Maybe Byte
  , uQ :: Maybe Byte -- the verilog does not make no-change explicit for Q
  , rQ :: Byte
  } deriving Eq

state2update :: State -> Update
state2update State{rPC,rA,rB,rX,rIR,rQ} =
  Update { uPC = Just rPC
         , uA = Just rA
         , uB = Just rB
         , uX = Just rX
         , uIR = Just rIR
         , uQ = Just rQ
         , rQ = rQ
         }

makeUpdate :: State -> State -> Update
makeUpdate
  State{rIR=ir1,rPC=pc1,rA=a1,rB=b1,rX=x1,rQ=q1}
  State{rIR=ir2,rPC=pc2,rA=a2,rB=b2,rX=x2,rQ=q2}
  =
  Update { uPC = mkUp pc1 pc2
         , uA = mkUp a1 a2
         , uB = mkUp b1 b2
         , uX = mkUp x1 x2
         , uIR = mkUp ir1 ir2
         , uQ = mkUp q1 q2
         , rQ = q2
         }
  where
    mkUp :: Eq a => a -> a -> Maybe a
    mkUp x y = if (x==y) then Nothing else Just y


instance Show Update where
  show Update{uPC,uA,uB,uX,uIR,uQ,rQ} = do
    printf  " %s | %s %s %s | %s | %s [%03d]"
      (see uIR) (see uA) (see uB) (see uX) (see uPC) (see uQ) rQ
    where
      see :: Maybe Byte -> String
      see = \case
        Nothing -> "~~"
        Just b -> printf "%02x" b

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
  NOP -> Cat o o FromZero ToInstructionRegister
  LZA -> Cat o o FromZero ToA
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
  JXS -> Cat x x FromX ToPC
  JIU -> Cat o o FromProgRom ToPC
  JIZ -> Cat o x FromProgRom ToPC
  JIC -> Cat x o FromProgRom ToPC
  JIS -> Cat x x FromProgRom ToPC
  ADD -> Cat o o FromAlu ToA
  TADD -> Cat o o FromAlu ToNowhere
  ADC -> Cat x o FromAlu ToA
  ADDB -> Cat o o FromAlu ToB
  ADDX -> Cat o o FromAlu ToX
  ADDM -> Cat o o FromAlu ToDataRam
  ADDOUT -> Cat o o FromAlu ToOut
  SUB -> Cat o x FromAlu ToA
  SBC -> Cat x x FromAlu ToA
  SUBB -> Cat o x FromAlu ToB
  SUBX -> Cat o x FromAlu ToX

  LSR -> Cat o o FromShiftedA ToA
  TLSR -> Cat o o FromShiftedA ToNowhere
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


data Source = FromZero | FromProgRom | FromA | FromB | FromX | FromDataRam | FromAlu | FromShiftedA
  deriving (Eq,Show)

encodeSource :: Source -> Byte
encodeSource = \case
  FromZero -> 0 -- drive zero onto bus
  FromProgRom -> 1
  FromA -> 2
  FromB -> 3
  FromX -> 4
  FromDataRam -> 5
  FromAlu -> 6
  FromShiftedA -> 7

decodeSource :: Byte -> Source
decodeSource = \case
  0 -> FromZero
  1 -> FromProgRom
  2 -> FromA
  3 -> FromB
  4 -> FromX
  5 -> FromDataRam
  6 -> FromAlu
  7 -> FromShiftedA
  x -> error (show ("decodeSource",x))


data Dest = ToInstructionRegister | ToDataRam | ToA | ToB | ToX | ToPC | ToOut | ToNowhere
  deriving (Eq,Show)

encodeDest :: Dest -> Byte
encodeDest =  \case
  ToInstructionRegister -> 0
  ToNowhere -> 1
  ToA -> 2
  ToB -> 3
  ToX -> 4
  ToDataRam -> 5
  ToOut -> 6
  ToPC -> 7

decodeDest :: Byte -> Dest
decodeDest = \case
  0 -> ToInstructionRegister
  1 -> ToNowhere
  2 -> ToA
  3 -> ToB
  4 -> ToX
  5 -> ToDataRam
  6 -> ToOut
  7 -> ToPC
  x -> error (show ("decodeDest",x))

----------------------------------------------------------------------
-- Control

data Control = Control
  { assertZero :: Bool
  , assertRom :: Bool
  , assertRam :: Bool
  , assertAlu :: Bool
  , assertShiftedA :: Bool
  , assertA :: Bool
  , assertB :: Bool
  , assertX :: Bool
  , loadIR :: Bool
  , loadPC :: Bool
  , loadA :: Bool
  , loadB :: Bool
  , loadX :: Bool
  , storeMem :: Bool
  , doOut :: Bool
  , doSubtract :: Bool
  , doCarryIn :: Bool
  , doShiftIn :: Bool
  , jumpIfZero :: Bool
  , jumpIfCarry :: Bool
  , jumpIfShift :: Bool
  , unconditionalJump :: Bool
  } deriving Show

cat2control :: Cat -> Control
cat2control = \case
  Lit{} -> error "unexpected Cat/Lit"
  Cat{xbit7,xbit3,dest,source} -> do
    let assertZero = (source == FromZero)
    let assertRom = (source == FromProgRom)
    let assertRam = (source == FromDataRam)
    let assertAlu = (source == FromAlu)
    let assertShiftedA = (source == FromShiftedA)
    let assertA = (source == FromA)
    let assertB = (source == FromB)
    let assertX = (source == FromX)
    let loadIR = (dest == ToInstructionRegister)
    let loadPC = (dest == ToPC)
    let loadA = (dest == ToA)
    let loadB = (dest == ToB)
    let loadX = (dest == ToX)
    let storeMem = (dest == ToDataRam)
    let doOut = (dest == ToOut)
    let doSubtract = xbit3
    let doCarryIn = xbit7
    let doShiftIn = xbit3
    let jumpIfZero = xbit3 && not xbit7
    let jumpIfCarry = not xbit3 && xbit7
    let jumpIfShift = xbit3 && xbit7
    let unconditionalJump = not xbit3 && not xbit7

    Control {assertZero,assertRom,assertRam,assertAlu,assertShiftedA
            ,assertA,assertB,assertX
            ,loadIR,loadPC,loadA,loadB,loadX,storeMem
            ,doOut,doSubtract,doCarryIn,doShiftIn
            ,jumpIfZero,jumpIfCarry,jumpIfShift,unconditionalJump}

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
  , rQ :: Byte
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
  , rQ = 0
  , flagCarry = False
  , flagShift = False
  }

initMem :: [Op] -> Map Byte Byte
initMem prog = Map.fromList (zip [0..] (map encodeOp prog))

encodeOp :: Op -> Byte
encodeOp = encodeCat . op2cat

data Output = Output Byte

stepWithControl :: State -> Control -> (State,Maybe Output)
stepWithControl state control = do
  let State{rom{-,ram-},rIR=_,rPC,rA,rB,rX,rQ,flagCarry,flagShift} = state
  let Control{assertZero,assertRom,assertRam,assertAlu,assertShiftedA
             ,assertA,assertB,assertX
             ,loadA,loadB,loadX,loadIR,loadPC,storeMem
             ,doOut,doSubtract,doCarryIn,doShiftIn
             ,jumpIfZero,jumpIfCarry,jumpIfShift,unconditionalJump} = control
  let aIsZero = (rA == 0)
  let jumpControl
        = (jumpIfZero && aIsZero)
        || (jumpIfCarry && flagCarry)
        || (jumpIfShift && flagShift)
        || unconditionalJump
  let alu = if doSubtract then (rA - rB) else (rA + rB + cin)
        where cin = if doCarryIn && flagCarry then 1 else 0
  let carry =
        if doSubtract
        then not (rB > rA)
        else fromIntegral rA + fromIntegral rB >= (256::Int)

  let aShifted =
        (if doShiftIn && flagShift then 128 else 0) + rA `div` 2
  let shiftedOut = rA `mod` 2 == 1
  let romOut = maybe 0 id (Map.lookup rPC rom)
  let dbus =
        case (assertZero,assertRom,assertRam,assertAlu
             ,assertA,assertB,assertX,assertShiftedA
             ) of
          (True,False,False,False,False,False,False,False) -> 0
          (False,True,False,False,False,False,False,False) -> romOut
          (False,False,True,False,False,False,False,False) -> maybe 0 id (Map.lookup rX rom)
          (False,False,False,True,False,False,False,False) -> alu
          (False,False,False,False,True,False,False,False) -> rA
          (False,False,False,False,False,True,False,False) -> rB
          (False,False,False,False,False,False,True,False) -> rX
          (False,False,False,False,False,False,False,True) -> aShifted
          (False,False,False,False,False,False,False,False) -> error "no drivers for data bus"
          p -> error (show ("multiple drivers for data bus",p))

  let doJump = loadPC && jumpControl
  let _ = loadIR
  let denyFetch = assertRom || doJump

  let s' = State
        { rom = if storeMem then Map.insert rX dbus rom else rom
        -- , ram = if storeMem then Map.insert rX dbus ram else ram
        , rIR = if denyFetch then 0 else romOut
        , rPC = if doJump then dbus else rPC + 1
        , rA = if loadA then dbus else rA
        , rB = if loadB then dbus else rB
        , rX = if loadX then dbus else rX
        , rQ = if doOut then dbus else rQ
        , flagCarry = if assertAlu then carry else flagCarry
        , flagShift = if assertShiftedA then shiftedOut else flagShift
        }
  (s',
   if doOut then Just (Output dbus) else Nothing
    )
