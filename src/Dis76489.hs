module Dis76489 (main) where

import Data.Word8 (Word8)

import Arc (Line(..),arcLineData)
-- explore disassembly of 76489 byte stream

main :: IO ()
main = do
  putStrLn "Dis76489"
  dis arcLineData
  pure ()


dis :: [Line] -> IO ()
dis lines = do
  --mapM_ print (take 5 lines)
  let voices = decode lines
  print voices


decode :: [Line] -> Voices
decode = loopLines time0 state0
  where

    loopLines :: Time -> State -> [Line] -> Voices
    loopLines time state = \case
      [] -> voicesNull
      Line{del,bytes}:lines -> do
        loopBytes state bytes

          where
            loopBytes :: State -> [Word8] -> Voices
            loopBytes state = \case
              [] -> do
                loopLines (delayTime del time) state lines
              byte:bytes -> do
                let command :: CommandByte = decodeByte byte
                let state' :: State = updateState state command
                pushCommand command (loopBytes state' bytes)


time0 :: Time
time0 = undefined

delayTime :: Int -> Time -> Time
delayTime = undefined

decodeByte :: Word8 -> CommandByte
decodeByte = undefined

pushCommand :: CommandByte -> Voices -> Voices
pushCommand = undefined

updateState :: State -> CommandByte -> State
updateState state@State{latchedReg} = \case
  LatchByte reg fourBits -> updateLatch state { latchedReg = reg } fourBits reg
  DataByte sixBits -> updateData state sixBits latchedReg

updateLatch :: State -> FourBits -> Reg -> State
updateLatch s@State{c1,c2,c3,cN} fourBits = \case
  Tone1 -> s { c1 = updateVoiceTone c1 fourBits }
  Tone2 -> s { c2 = updateVoiceTone c2 fourBits }
  Tone3 -> s { c3 = updateVoiceTone c3 fourBits }
  Att1 -> s { c1 = updateVoiceAtt c1 fourBits }
  Att2 -> s { c2 = updateVoiceAtt c2 fourBits }
  Att3 -> s { c3 = updateVoiceAtt c3 fourBits }
  NoiseAtt -> s { cN = cN { att = parseAtt fourBits } }
  NoiseControl -> s { cN = cN { control = parseNoiseControl fourBits } }

updateData :: State -> SixBits -> Reg -> State
updateData = undefined

updateVoiceTone :: Voice -> FourBits -> Voice
updateVoiceTone = undefined

updateVoiceAtt :: Voice -> FourBits -> Voice
updateVoiceAtt = undefined

parseNoiseControl :: FourBits -> NoiseControl
parseNoiseControl = undefined

parseAtt :: FourBits -> Attenuation
parseAtt = undefined

data State = State
  { latchedReg :: Reg
  , c1 :: Voice
  , c2 :: Voice
  , c3 :: Voice
  , cN :: Noise
  }

state0 :: State
state0 = State { latchedReg, c1 = voice0, c2 = voice0, c3 = voice0, cN = noise0 }
  where
    latchedReg :: Reg
    latchedReg = undefined
    voice0 :: Voice
    voice0 = Voice { att = att0, tone = tone0 }

    noise0 :: Noise
    noise0 = undefined

    att0 :: Attenuation
    att0 = undefined

    tone0 :: Tone
    tone0 = undefined


voicesNull :: Voices
voicesNull =  Voices
  { c1 = streamNull
  , c2 = streamNull
  , c3 = streamNull
  , cN = streamNull
  }

streamNull :: Stream a
streamNull = Stream []

data CommandByte
  = LatchByte Reg FourBits
  | DataByte SixBits

data Reg = Tone1 | Att1 | Tone2 | Att2 | Tone3 | Att3 | NoiseControl | NoiseAtt

data Voices = Voices
  { c1 :: Stream Voice
  , c2 :: Stream Voice
  , c3 :: Stream Voice
  , cN :: Stream Noise
  }
  deriving Show


data Voice = Voice { att :: Attenuation, tone :: Tone }
  deriving Show

data Noise = Noise { att :: Attenuation, control :: NoiseControl }
  deriving Show

data NoiseControl = NoideControl { fb :: NoiseFeedback, gen :: NoiseGen }
  deriving Show

data NoiseFeedback = Periodic | White
  deriving Show

data NoiseGen = Rate512 | Rate1024 | Rate2048 | RateC3
  deriving Show

newtype Attenuation = Attenuation FourBits
  deriving Show

newtype Tone = Tone TenBits
  deriving Show

type FourBits = Int
type SixBits = Int
type TenBits = Int


data Stream a = Stream [(Time,a)]
  deriving Show

newtype Time = Time { stepsFromStart :: Int }
  deriving Show
