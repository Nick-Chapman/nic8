module Top (main) where

import Arc (generateSoundData)
import Asm (Op,Byte)
import Control.Monad (forM_)
import Data.List (intercalate)
import qualified Examples (table)
import Text.Printf (printf)
import qualified Emu (runCollectOutput,encodeOp)
import qualified Rom2k (generateAll)
import qualified Test (run)
import qualified Dis76489 as Dis (main)

main :: IO ()
main = do
  let _ = Dis.main
  let _ = Arc.generateSoundData
  Test.run -- regression tests
  printAndRunExamples Examples.table
  Rom2k.generateAll
  pure ()

printAndRunExamples :: [(String,[Op])] -> IO ()
printAndRunExamples examples = do
  forM_ examples $ \(name,prog) -> do
    printProg name prog
    print $ Emu.runCollectOutput 500 prog
    pure ()

printProg :: String -> [Op] -> IO ()
printProg name prog = do
  printf "%s:\n" name
  forM_ (zip [0::Byte ..] prog) $ \(i,op) -> do
    let b = Emu.encodeOp op
    printf "%3d: %08b : (0x%02x) %08b : %s\n" i i b b(show op)
  printf "int program[] = {%s};\n"
    (intercalate ", "(map (\op -> printf "0x%02x" (Emu.encodeOp op) :: String) prog))
