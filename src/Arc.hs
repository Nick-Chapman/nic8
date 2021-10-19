
module Arc (generateSoundData) where

import ArcRawData (rawData)
import Data.Bits (testBit,(.|.),bit)
import Data.Word8 (Word8)
import Text.Printf (printf)

generateSoundData :: IO ()
generateSoundData = do
  putStrLn "*generateSoundData*"
  print (length rawData)
  let collected = collect rawData
  print (length collected)
  mapM_ print collected
  pure ()

data Line = Line { del :: Int, bytes :: [Word8] }

instance Show Line where
  show (Line{del,bytes}) =
    printf "    .byte %d, %d%s" del (length bytes)
    (concat [ printf ", $%02x" (flipByte byte) :: String | byte <- bytes ])

-- Flip bits in a byte from left-to-right
-- TODO: remove when I fix the wiring between 6522 and 76489 !
flipByte :: Word8 -> Word8
flipByte b =
  foldl1 (.|.) [ setOrClear (b `testBit` n) (7-n)  | n <- [0..7] ]
  where
    setOrClear :: Bool -> Int -> Word8
    setOrClear b n = if b then bit n else 0


collect :: [(Int,Word8)] -> [Line]
collect = \case
  [] -> error "collect[]"
  (n,x):rest -> loop 0 n [x] rest
  where
    loop del n0 acc = \case
      [] -> [Line del (reverse acc)]
      (n1,x):rest ->
        if n1==n0
        then loop del n0 (x:acc) rest
        else Line del (reverse acc) : loop (n1-n0) n1 [x] rest
