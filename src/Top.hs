module Top (main) where

import qualified Test (run)

main :: IO ()
main = do
  putStrLn "*SAP*"
  -- run the regression tests
  Test.run
