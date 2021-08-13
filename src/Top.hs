module Top (main) where

--import Examples
--import qualified Emu (runIO)
import qualified Test (run)

main :: IO ()
main = do
  putStrLn "*SAP*"

  -- run the regression tests
  Test.run

  --putStrLn "*Running example under dev*"
  --let prog = multiply5by7
  --Emu.runIO prog
