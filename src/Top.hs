module Top (main) where

import Asm (Op,Byte)
import Control.Monad (forM_)
import Examples
import Text.Printf (printf)
import qualified Emu (runIO,runCollectOutput,encodeOp)
import qualified Primes (outputPrimes)
import qualified Test (run)

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
  let _prog = countdownForever
  let _prog = fib3vars
  let _prog = fib2vars
  let prog = varProg1
  printProg prog

  let _ = Emu.runIO prog
  print $ Emu.runCollectOutput 100 prog
  pure ()

printProg :: [Op] -> IO ()
printProg prog = do
  printf "#prog = %d\n" (length prog)
  forM_ (zip [0::Byte ..] prog) $ \(i,op) ->
    printf "%3d: %08b : %08b : %s\n" i i (Emu.encodeOp op) (show op)
