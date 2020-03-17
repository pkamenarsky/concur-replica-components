module Spec where

import Concur.Replica.Components.Grid

import Control.Monad
import Control.Monad.ST

import Data.Array.ST
import Data.List.Split

import Test.QuickCheck
import System.Random

import Debug.Trace

genRect :: Random a => a -> a -> Int -> Int -> Int -> Int -> Gen (RectA a)
genRect a0 a1 x0 y0 w h = do
  a  <- choose (a0, a1)
  x1 <- choose (x0, w)
  y1 <- choose (y0, h)
  x2 <- choose (x1, w)
  y2 <- choose (y1, h)
  pure (a, P x1, P y1, P x2, P y2)

fillRect :: STArray s (Int, Int) a -> RectA a -> ST s ()
fillRect array (a, P x1, P y1, P x2, P y2) = sequence_
  [ writeArray array (y, x) a
  | x <- [x1..x2 - 1]
  , y <- [y1..y2 - 1]
  ]

arrayToLists :: STArray s (Int, Int) a -> ST s [[a]]
arrayToLists array = do
  ((_, y), (_, h)) <- getBounds array
  es <- getElems array
  pure $ chunksOf (h - y + 1) es

fillRects :: a -> Int -> Int -> [RectA a] -> ST s [[a]]
fillRects defa w h rects = do
  array <- newArray ((0, 0), (h - 1, w - 1)) defa
  mapM_ (fillRect array) rects
  arrayToLists array

genLists :: Random a => a -> a -> a -> Int -> Int -> Gen [[a]]
genLists defa a0 a1 w h = do
  rects <- replicateM 10 (genRect a0 a1 0 0 w h)
  pure $ runST $ fillRects defa w h rects

rects_prop :: Int -> Int -> [[Int]] -> Property
rects_prop w h as
  = tabulate "Rect values" values
  $ tabulate "Rect size" sizes (as == rso)
  where
    rsi = rectsBy (==) as
    rso = runST $ fillRects (-1) w h rsi

    sizes  = [ show w ++ "x" ++ show h | (_, _, _, P w, P h) <- rsi ]
    values = [ show a | (a, _, _, _, _) <- rsi ]

main :: IO ()
main = do
  quickCheck (forAll (genLists 0 5 (-1) w h) (rects_prop w h))
  quickCheck (forAll (genLists 0 10 (-1) w' h') (rects_prop w' h'))
  quickCheck (forAll (genLists 0 50 (-1) w h) (rects_prop w h))
  where
    w = 20
    h = 10

    w' = 50
    h' = 40
