module Rom2k (generateAll) where

import Prelude hiding (take)
import qualified Data.ByteString as BS
import Data.Word8 (Word8)
import Data.Bits (shiftL)

take :: Int -> [a] -> [a]
take n = loop (0::Int) n
  where
    loop _ 0 _ = []
    loop i n xs = case xs of
      [] -> error (show ("take, not enough, have",i,"need another",n))
      x:xs -> x : loop (i+1) (n-1) xs

generateAll :: IO ()
generateAll = do
  putStrLn "*generating roms*"
  genRom2k "zero" $ replicate 2048 0x00
  genRom2k "erased" $ replicate 2048 0xff
  genRom2k "decimalLED" decimalLED
  genRom2k "hexLED" hexLED
  genRom2k "decimalAndHexLED" decimalAndHexLED

decimalLED :: [Word8]
decimalLED = concat
  [ erasedPage
  , take 256 (concat (repeat ddigs))
  , take 256 (concat (repeat (concat $ map (replicate 10) ddigs)))
  , take 256 (concat (repeat (concat $ map (replicate 100) ddigs)))
  , erasedPage
  , erasedPage
  , erasedPage
  , erasedPage
  ]

hexLED :: [Word8]
hexLED = concat
  [ erasedPage
  , erasedPage
  , erasedPage
  , erasedPage
  , erasedPage
  , erasedPage
  , concat (replicate 16 hdigs)
  , concat (map (replicate 16) hdigs)
  ]

decimalAndHexLED :: [Word8]
decimalAndHexLED = concat
  [ erasedPage
  , take 256 (concat (repeat ddigs))
  , take 256 (concat (repeat (concat $ map (replicate 10) ddigs)))
  , take 256 (concat (repeat (concat $ map (replicate 100) ddigs)))
  , erasedPage
  , erasedPage
  , concat (replicate 16 hdigs)
  , concat (map (replicate 16) hdigs)
  ]

erasedPage :: [Word8]
erasedPage = replicate 256 0xff

ddigs :: [Word8]
--ddigs = take 10 hdigs
ddigs = [ encodeSegs (segs i) | i <- [0..9] ]

hdigs :: [Word8]
hdigs = [0x3f, 0x0c, 0x5b, 0x5e, 0x6c, 0x76, 0x77, 0x1c, 0x7f, 0x7c
        ,1,2,3,4,5,6] -- TODO: determine LED code for hex values: a,b,c,d,e,f

data Seg = A | B | C | D | E | F | G deriving Eq
{-
                      A
                    F   B
                      G
                    E   C
                      D
-}
segs :: Int -> [Seg]
segs = \case
  0 -> [ A, B, C, D, E, F    ]
  1 -> [    B, C             ]
  2 -> [ A, B,    D, E,    G ]
  3 -> [ A, B, C, D,       G ]
  4 -> [    B, C,       F, G ]
  5 -> [ A,    C, D,    F, G ]
  6 -> [ A,    C, D, E, F, G ]
  7 -> [ A, B, C             ]
  8 -> [ A, B, C, D, E, F, G ]
  9 -> [ A, B, C,       F, G ]
  n ->
    error (show ("segs",n))

encodeSegs :: [Seg] -> Word8
encodeSegs segs = sum [ 1 `shiftL` segBit seg | seg <- segs ]

segBit :: Seg -> Int
segBit seg = the [ n | (x,n) <- bitMap, x == seg]
  where
    bitMap = zip order [0..]
    order = [E,D,C,B,A,F,G]

the :: Show a => [a] -> a
the = \case
  [x] -> x
  xs -> error (show ("the",xs))

genRom2k :: String -> [Word8] -> IO ()
genRom2k name bytes = do
  let n = length bytes
  if n == 2048 then pure () else error (show ("genRom2k",name,"wrong-size",n))
  --putStrLn $ "- " ++ name
  BS.writeFile ("roms/" ++ name ++ ".rom") (BS.pack bytes)

