module Top (main) where

import Arc (generateSoundData)
import Asm (Op,Byte)
import Control.Monad (forM_)
import Data.List (intercalate,sortBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Dis76489 as Dis (main)
import qualified Emu (runCollectOutput,encodeOp)
import qualified Examples (table)
import qualified Op (Op(NOP),allOps)
import qualified Rom2k (generateAll,pad)
import qualified Test (run)

main :: IO ()
main = do
  args <- getArgs
  let config = parseCommandLine args
  run config

data Config = Config { actions :: [Action] }

data Action
  = DisMain
  | GenerateSoundData
  | GenerateRoms
  | RegressionTests
  | GenerateAsmCrib
  | AssembleExampleTable
  | PrintAndRunAllExamples
  | PrintAndRun String

parseCommandLine :: [String] -> Config
parseCommandLine = loop []
  where
    loop :: [Action] -> [String] -> Config
    loop acc = \case
      [] -> Config { actions = reverse acc }
      "dis":xs -> loop (DisMain : acc) xs
      "sound":xs -> loop (GenerateSoundData : acc) xs
      "roms":xs -> loop (GenerateRoms : acc) xs
      "tests":xs -> loop (RegressionTests : acc) xs
      "crib":xs -> loop (GenerateAsmCrib : acc) xs
      "assemble-examples":xs -> loop (AssembleExampleTable : acc) xs
      "runall":xs -> loop (PrintAndRunAllExamples : acc) xs
      "run":sel:xs -> loop (PrintAndRun sel : acc) xs
      x:_ -> error (show ("parseCommandLine",x))

run :: Config -> IO ()
run Config{actions} = do
  case actions of
    [] -> putStrLn "*nothing to do*"
    _ -> mapM_ enact actions

enact :: Action -> IO ()
enact = \case
  DisMain -> do
    Dis.main
  GenerateSoundData -> do
    Arc.generateSoundData
  GenerateRoms -> do
    Rom2k.generateAll
  RegressionTests -> do
    Test.run
  GenerateAsmCrib -> do
    generateAsmCrib
  AssembleExampleTable -> do
    assembleExamples Examples.table
  PrintAndRunAllExamples -> do
    printAndRunExamples Examples.table
  PrintAndRun sel -> do
    printAndRunExamples [ ex | ex@(name,_) <- Examples.table, name == sel ]

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

generateAsmCrib :: IO ()
generateAsmCrib = do
  let filename = "asm.encoding"
  printf "Generating: %s\n" filename
  writeFile filename $ unlines
    [ printf "%02x : %s" b (show op) :: String
    | (b,op) <- sortBy (comparing fst) [ (Emu.encodeOp op,op) | op <- Op.allOps ]
    ]

assembleExamples :: [(String,[Op])] -> IO () -- for verilog sim
assembleExamples examples = do
  forM_ (zip [0::Int ..] examples) $ \(i,(name,ops)) -> do
    let filename = "prog/" ++ name ++ ".hex"
    printf "writing: %s\n" filename
    writeFile filename (commentedVerilogHexDump i name ops)

commentedVerilogHexDump :: Int -> String -> [Op] -> String
commentedVerilogHexDump _i name ops = do
  let banner = printf "// %s" name
  let hex = [ intercalate " " [ printf "%02x" (Emu.encodeOp op) | op <- ops ]
            | ops <- chunksOf 16 (Rom2k.pad Op.NOP 256 ops)
            ]
  let ass = [ printf "%3d: %08b : (0x%02x) %08b : %s" i i b b (show op) :: String
            | (i,op) <- zip [0::Byte ..] ops
            , let b = Emu.encodeOp op
            ]
  unlines $ [banner] ++ hex ++ ["/*"] ++ ass ++ ["*/"]
