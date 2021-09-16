module Top (main) where

import Asm (Op,Byte)
import Control.Monad (forM_)
import Examples
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
      [ ("fibForever",fibForever)
      , ("countdownForever",countdownForever)
      , ("openCountLoop",openCountLoop)
      , ("tightCountLoop",tightCountLoop)
      , ("varProg0",varProg0)
      ]
  forM_ examples $ \(name,prog) -> do
    printProg name prog
    print $ Emu.runCollectOutput 300 prog
    pure ()

printProg :: String -> [Op] -> IO ()
printProg name prog = do
  printf "%s:\n" name
  forM_ (zip [0::Byte ..] prog) $ \(i,op) ->
    printf "%3d: %08b : %08b : %s\n" i i (Emu.encodeOp op) (show op)
