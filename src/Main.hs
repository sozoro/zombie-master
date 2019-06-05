-- {-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Matrix as M
import Control.Monad
import Control.Applicative
import Data.Maybe (listToMaybe,catMaybes)

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
  maybe (M.unsafeGet r c m) id $ guard (r == x) >> ls `safeIx` (fixed - c)
  where
    fixed = downer (M.nrows m) y
    ls    = f $ catMaybes $ map (\y' -> M.safeGet x y' m) $ enumToFrom fixed 1

modifyU :: ([a] -> [a]) -> (Int, Int) -> M.Matrix a -> M.Matrix a
modifyU f (x, y) m = M.matrix (M.nrows m) (M.ncols m) $ \(r,c) ->
  maybe (M.unsafeGet r c m) id $ guard (c == y) >> ls `safeIx` (fixed - r)
  where
    fixed = downer (M.ncols m) x
    ls    = f $ catMaybes $ map (\x' -> M.safeGet x' y m) $ enumToFrom fixed 1

modifyR :: ([a] -> [a]) -> (Int, Int) -> M.Matrix a -> M.Matrix a
modifyR f (x, y) m = M.matrix (M.nrows m) (M.ncols m) $ \(r,c) ->
  maybe (M.unsafeGet r c m) id $ guard (r == x) >> ls `safeIx` (c - fixed)
  where
    fixed = upper 1 y
    ls    = f $ catMaybes $ map (\y' -> M.safeGet x y' m)
                          $ enumFromTo fixed (M.nrows m)

upper :: Ord a => a -> a -> a
upper a b = if a >= b then a else b

downer :: Ord a => a -> a -> a
downer a b = if a <= b then a else b

enumToFrom :: Enum a => a -> a -> [a]
enumToFrom a = enumFromThenTo a $ pred a

safeIx :: [a] -> Int -> Maybe a
safeIx ls i = guard (i >= 0) >> listToMaybe (drop i ls)

_4x4 :: M.Matrix Clockwise
_4x4 = M.fromList 4 4 $ iterate cyclicSucc L

main :: IO ()
main = putStrLn "Hello, Haskell!"
