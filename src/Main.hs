-- {-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Matrix as M
import Control.Monad
import Control.Applicative
import Data.Maybe (listToMaybe)

data Clockwise = L | U | R | D deriving (Eq, Enum, Bounded)
instance Show Clockwise where
  show L = "←"
  show U = "↑"
  show R = "→"
  show D = "↓"

data Player = LeftP | RightP deriving (Show, Eq, Enum, Bounded)

cyclicSucc :: forall a. (Enum a, Bounded a) => a -> a
cyclicSucc a | fromEnum a >= fromEnum (maxBound :: a) = minBound
             | otherwise                              = succ a

cyclicPred :: forall a. (Enum a, Bounded a) => a -> a
cyclicPred a | fromEnum a <= fromEnum (minBound :: a) = maxBound
             | otherwise                              = pred a

cyclicToEnum :: forall a. (Enum a, Bounded a) => Int -> a
cyclicToEnum i = toEnum $ i `mod` length ([minBound .. maxBound :: a])

modifyL :: ([a] -> [a]) -> (Int, Int) -> M.Matrix a -> M.Matrix a
modifyL f (x, y) m = M.matrix (M.nrows m) (M.ncols m) $ \(r,c) ->
  maybe (M.unsafeGet r c m) id $ guard (c == y) >> listToMaybe (drop r ls)
  where
    ls = f $ map (\x' -> M.getElem x' y m) [x .. M.nrows m]

_4x4 :: M.Matrix Clockwise
_4x4 = M.fromList 4 4 $ iterate cyclicSucc L

main :: IO ()
main = putStrLn "Hello, Haskell!"
