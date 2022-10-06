module Top (main) where

import Arc (generateSoundData)
import Asm (Op,Byte)
import Control.Monad (forM_)
import Data.List (intercalate,sortBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Data.Word8 (Word8)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Dis76489 as Dis (main)
import qualified Emu (encodeOp,sim,run)
import qualified Examples (table,rom1,rom2)
import qualified Op (Op(NOP),allOps)
import qualified Rom2k (generateAll)
import Rom2k (genRom2k,pad,erasedPage)
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
  | GenerateProgRom1
  | GenerateProgRom2
  | RegressionTests
  | GenerateAsmCrib
  | AssembleExampleTable
  | Simulate String
  | SimulateExampleTable
  | Run String
  | RunExampleTable

parseCommandLine :: [String] -> Config
parseCommandLine = loop []
  where
    loop :: [Action] -> [String] -> Config
    loop acc = \case
      [] -> Config { actions = reverse acc }
      "dis":xs -> loop (DisMain : acc) xs
      "sound":xs -> loop (GenerateSoundData : acc) xs
      "roms":xs -> loop (GenerateRoms : acc) xs
      "rom1":xs -> loop (GenerateProgRom1 : acc) xs
      "rom2":xs -> loop (GenerateProgRom2 : acc) xs
      "tests":xs -> loop (RegressionTests : acc) xs
      "crib":xs -> loop (GenerateAsmCrib : acc) xs
      "assemble-examples":xs -> loop (AssembleExampleTable : acc) xs
      "sim":sel:xs -> loop (Simulate sel : acc) xs
      "simall":xs -> loop (SimulateExampleTable : acc) xs
      "run":sel:xs -> loop (Run sel : acc) xs
      "runall":xs -> loop (RunExampleTable : acc) xs
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
  GenerateProgRom1 -> do
    genProgRom "programs1" Examples.rom1
  GenerateProgRom2 -> do
    genProgRom "programs2" Examples.rom2
  RegressionTests -> do
    Test.run
  GenerateAsmCrib -> do
    generateAsmCrib
  AssembleExampleTable -> do
    assembleExamples Examples.table
  Simulate sel -> do
    mapM_ simExample [ ex | ex@(name,_) <- Examples.table, name == sel ]
  SimulateExampleTable -> do
    simulateExamples Examples.table
  Run sel -> do
    mapM_ runExample [ ex | ex@(name,_) <- Examples.table, name == sel ]
  RunExampleTable -> do
    runExamples Examples.table



genProgRom :: String -> [(String,[Op])] -> IO ()
genProgRom name table =
  genRom2k name (compileProgsTable table)

compileProgsTable :: [(String,[Op])] -> [Word8]
compileProgsTable table =
  concat (pad erasedPage 8 [compileOps ops | (_,ops) <- table])

compileOps :: [Op] -> [Word8]
compileOps ops = pad 0x0 256 (map Emu.encodeOp ops)


simulateExamples :: [(String,[Op])] -> IO () -- and save to files
simulateExamples examples = do
  forM_ examples $ \(name,prog) -> do
    let filename = "_gen/" ++ name ++ ".trace"
    printf "writing: %s\n" filename
    writeFile filename (unlines (Emu.sim 150 prog))

simExample :: (String,[Op]) -> IO ()
simExample (name,prog) = do
  printf "%s:\n" name
  mapM_ putStrLn $ Emu.sim 300 prog


runExamples :: [(String,[Op])] -> IO () -- and save to files
runExamples examples = do
  forM_ examples $ \(name,prog) -> do
    let filename = "_gen/" ++ name ++ ".out"
    printf "writing: %s\n" filename
    writeFile filename (unlines $ map (printf "%03d") $ Emu.run 1500 prog)

runExample :: (String,[Op]) -> IO ()
runExample (name,prog) = do
  printf "%s:\n" name
  mapM_ (printf "%03d\n") $ Emu.run 5000 prog


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
  let ass = [ printf "%3d (0x%02x) %08b : (0x%02x) %08b : %s" i i i b b (show op) :: String
            | (i,op) <- zip [0::Byte ..] ops
            , let b = Emu.encodeOp op
            ]
  unlines $ [banner] ++ hex ++ ["/*"] ++ ass ++ ["*/"]
