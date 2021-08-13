
module Testing (test,run) where

import Control.Monad (ap,liftM)
import Emu (Cycles(..),OutOfGas(..),runCollectOutput)
import Op (Op,Byte)

test :: [Op] -> [Byte] -> Testing ()
test prog expected = T1 (Test prog expected)

run :: Testing () -> IO ()
run testing = do
  bools <- sequence [ runTest i x | (i,x) <- zip [1..] (collect testing) ]
  let numTests = length bools
  let numPass = length [ () | res <- bools, res ]
  let numFail = numTests - numPass
  putStrLn $
    show numTests ++ " tests ran; " ++ (if numFail > 0 then show numFail ++ " fail." else "all pass.")

instance Functor Testing where fmap = liftM
instance Applicative Testing where pure = return; (<*>) = ap
instance Monad Testing where return = Ret; (>>=) = Bind

data Testing a where
  Ret :: a -> Testing a
  Bind :: Testing a -> (a -> Testing b) -> Testing b
  T1 :: Test -> Testing ()

collect :: Testing () -> [Test]
collect m = loop m $ \_ -> [] where
  loop :: Testing a -> (a -> [Test]) -> [Test]
  loop m k = case m of
    Ret a -> k a
    Bind m f -> loop m $ \a -> loop (f a) k
    T1 x -> x : k ()

data Test = Test [Op] [Byte]

instance Show Test where
  show (Test is xs) = "input: " ++ show is ++ "\n- expect: " ++ show xs

runTest :: Int -> Test -> IO Bool
runTest n t@(Test prog expected) = do
  let max = Cycles 500
  case runCollectOutput max prog of
    Left OutOfGas -> do
      putStrLn $ "test #" ++ show n ++ ", " ++ show t
      putStrLn $ "- *OutOfGas*: (exceeded " ++ show max ++ " cycles)"
      pure False
    Right actual ->
      if actual == expected then pure True else do
        putStrLn $ "test #" ++ show n ++ ", " ++ show t
        putStrLn $ "- actual: " ++ show actual
        pure False
