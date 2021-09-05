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


varProg1 :: [Op] -- small program which does something useful with memory
varProg1 = assemble $ mdo
  start <- Here
  loadA v1
  out
  lb 1
  add
  storeA v1
  la 0
  out
  jump start
  v1 <- variable 42
  pure ()

fib2vars :: [Op] -- program which uses memory for variable storage (2 vars)
fib2vars = assemble $ mdo
  start <- Here
  la 1
  storeA q
  la 0
  storeA p
  loop <- Here
  loadB p
  loadA q
  storeA p
  add
  storeA q
  out
  lb 233; sub
  jz start --Emit [TAX, JXZ]
  jump loop
  pure ()
  p <- variable 0
  q <- variable 0
  pure ()

fib3vars :: [Op] -- program which uses memory for variable storage (3 vars)
fib3vars = assemble $ mdo
  start <- Here
  la 0
  storeA p
  la 1
  storeA q
  loop <- Here
  loadA p
  loadB q
  add
  storeA r
  loadA q
  storeA p
  loadA r
  storeA q
  out
  lb 233; sub; jz start
  jump loop
  p <- variable 0
  q <- variable 0
  r <- variable 0
  pure ()


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
