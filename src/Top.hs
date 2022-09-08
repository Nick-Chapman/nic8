module Top (main) where

import Arc (generateSoundData)
import Asm (Op,Byte)
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Text.Printf (printf)
import qualified Dis76489 as Dis (main)
import qualified Emu (runCollectOutput,encodeOp)
import qualified Examples (table)
import qualified Op (Op(NOP))
import qualified Rom2k (generateAll,pad)
import qualified Test (run)

main :: IO ()
main = do
  let _ = Dis.main
  let _ = Arc.generateSoundData
  let _ = Rom2k.generateAll
  let _ = printAndRunExamples Examples.table
  Test.run -- regression tests
  assembleExamples Examples.table
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

assembleExamples :: [(String,[Op])] -> IO () -- for verilog sim
assembleExamples examples = do
  forM_ examples $ \(name,ops) -> do
    let filename = "prog/" ++ name ++ ".hex"
    printf "writing: %s\n" filename
    writeFile filename (commentedVerilogHexDump ops)

commentedVerilogHexDump :: [Op] -> String
commentedVerilogHexDump ops = do
  let hex = [ intercalate " " [ printf "%02x" (Emu.encodeOp op) | op <- ops ]
            | ops <- chunksOf 16 (Rom2k.pad Op.NOP 256 ops)
            ]
  let ass = [ printf "%3d: %08b : (0x%02x) %08b : %s" i i b b (show op) :: String
            | (i,op) <- zip [0::Byte ..] ops
            , let b = Emu.encodeOp op
            ]
  unlines $ hex ++ ["/*"] ++ ass ++ ["*/"]
