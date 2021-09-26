module Top (main) where

import Asm --(Op,Byte)
import Control.Monad (forM_)
import Data.List (intercalate)
--import Examples
import Primes (primes)
import Text.Printf (printf)
import qualified Emu
import qualified Test (run)

main :: IO ()
main = do
  putStrLn "*nic8*"
  -- regression tests
  Test.run
  putStrLn "Assembled examples..."
  let
    examples =
      [ {-("fibForever",fibForever)
      , ("openCountLoop",openCountLoop)
      , ("tightCountLoop",tightCountLoop)
      , ("varProg0",varProg0)
      , ("countdownForeverZ",countdownForeverZ)
      , ("countdownForeverC",countdownForeverC)
      , -}("primes",primes False)
      ]
  forM_ examples $ \(name,prog) -> do
        printProg name prog
        print $ Emu.runCollectOutput 500 prog
        pure ()
  pure ()

printProg :: String -> [Op] -> IO ()
printProg name prog = do
  printf "%s:\n" name
  {-forM_ (zip [0::Byte ..] prog) $ \(i,op) -> do
    let b = Emu.encodeOp op
    printf "%3d: %08b : (0x%02x) %08b : %s\n" i i b b(show op)-}
  printf "int program[] = {%s};\n"
    (intercalate ", "(map (\op -> printf "0x%02x" (Emu.encodeOp op) :: String) prog))
