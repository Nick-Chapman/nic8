module Top (main) where

import Control.Monad (forM_)
import Text.Printf (printf)
import qualified Emu (runIO,runCollectOutput,encodeOp)
import qualified Examples
import qualified Primes
import qualified Test (run)
import Asm

main :: IO ()
main = do
  putStrLn "*SAP*"
  -- run the regression tests
  Test.run

  putStrLn "Running example..."
  let _prog = vSmall
  let _prog = fibForever
  let _prog = fibUnrolled
  let _prog = Examples.fibC
  let _prog = Primes.outputPrimes
  let prog = countdownForever
  printProg prog

  let _ = Emu.runIO prog
  print $ Emu.runCollectOutput 100 prog
  pure ()

printProg :: [Op] -> IO ()
printProg prog = do
  printf "#prog = %d\n" (length prog)
  forM_ (zip [0::Byte ..] prog) $ \(i,op) ->
    printf "%3d: %08b : %08b : %s\n" i i (Emu.encodeOp op) (show op)


countdownForever :: [Op]
countdownForever = assemble $ do
  lb 1
  start <- Here
  la 5
  loop <- Here
  out
  jz start
  sub
  jump loop

vSmall :: [Op]
vSmall = assemble $ do
  la 21
  tab
  add
  out
  halt

fibForever :: [Op]
fibForever = assemble $ mdo
  la 1
  loop <- Here
  out; addx
  tab; txa
  jump loop

fibUnrolled :: [Op]
fibUnrolled = assemble $ mdo
  lb 1
  addout
  add
  addout
  addb
  addout
  add
  addout
  addb
  addout
  add
  addout
  addb
  halt
