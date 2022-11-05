
-- play with circuit building combinators
module Circuits where

main :: IO ()
main = do
  print "*Circuits*"

  let
    example :: Circ Bit Bit
    example = loopRight (compose ff inv)

  print $ run' example [Lo,Hi,Lo,Lo,Hi,Hi]
  pure ()


compose :: Circ x y -> Circ y z -> Circ x z
compose c1 = loopR
  where
    loopR = \case
      Put z c2 -> Put z (loopR c2)
      Get f2 -> loopL f2 c1
        where
          loopL f2 = \case
            Put y c1 -> compose c1 (f2 y)
            Get f1 -> do
              Get $ \x ->
                loopL f2 (f1 x)


loopLeft :: Circ (Either a b) a -> Circ b a
loopLeft = loop Nothing
  where
    loop opt = \case
      Get f -> do
        case opt of
          Nothing -> Get $ \b -> loop Nothing (f (Right b))
          Just a -> loop Nothing (f (Left a))
      Put x me ->
        Put x $ loop (Just x) me

loopRight :: Circ (Either a b) b -> Circ a b
loopRight = loop Nothing
  where
    loop opt = \case
      Get f -> do
        case opt of
          Nothing -> Get $ \a -> loop Nothing (f (Left a))
          Just b -> loop Nothing (f (Right b))
      Put x me ->
        Put x $ loop (Just x) me


ff :: Circ (Either Bit Bit) Bit
ff = splitInputPair Lo Lo (me Lo Lo)
  where
    me :: Bit -> Bit -> Circ (Bit,Bit) Bit
    me q lastClk = do
      Get $ \(clk,d) ->
        case (lastClk,clk) of
          (Lo,Hi) -> Put d $ me d clk
          _ -> me q clk


splitInputPair :: a -> b -> Circ (a,b) o -> Circ (Either a b) o
splitInputPair a b c = loop a b c
  where
    loop a b = \case
      Put o c -> Put o (loop a b c)
      Get f -> do
        Get $ \case
          Left a -> loop a b (f (a,b))
          Right b -> loop a b (f (a,b))

inv :: Circ Bit Bit
inv = me where
  me = do
    Get $ \x ->
      Put (invb x) me

nand :: Circ (Bit,Bit) Bit
nand = me where
  me = do
    Get $ \(x,y) ->
      Put (nandb x y) me

data Circ i o
  = Get (i -> Circ i o)
  | Put o (Circ i o)

run :: Circ i o -> [i] -> [o]
run circ0 is = loop is circ0
  where
    loop :: [i] -> Circ i o -> [o]
    loop is = \case
      Put o c -> o : loop is c
      Get f -> case is of [] -> []; i:is -> loop is (f i)

run' :: forall i o. Circ i o -> [i] -> ([o],[(i,[o])])
run' circ0 is0 = loop0 [] circ0
  where
    loop0 :: [o] -> Circ i o -> ([o],[(i,[o])])
    loop0 os = \case
      Put o c ->  loop0 (o:os) c
      Get f ->
        case is0 of
          [] -> (reverse os, [])
          i:is -> (reverse os, loop i [] is (f i))

    loop :: i -> [o] -> [i] -> Circ i o -> [(i,[o])]
    loop iLast os is = \case
      Put o c -> loop iLast (o:os) is c
      Get f ->
        case is of
          [] -> [(iLast,reverse os)]
          i:is -> (iLast,reverse os) : loop i [] is (f i)


data Bit = Hi | Lo deriving Show

invb :: Bit -> Bit
invb = \case Hi -> Lo; Lo -> Hi

nandb :: Bit -> Bit -> Bit
nandb = \case
  Lo -> \_ -> Hi
  Hi -> invb
