-- {-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Maybe (listToMaybe,catMaybes)
import qualified Data.Matrix       as M
import qualified Data.Vector       as V
import qualified System.Random.MWC as R

data Clockwise = L | U | R | D deriving (Eq, Enum, Bounded)
instance Show Clockwise where
  show L = "←"
  show U = "↑"
  show R = "→"
  show D = "↓"

data Player = Blue | Red deriving (Eq, Enum, Bounded)
instance Show Player where
  show Blue = "B"
  show Red  = "R"

data Zombie
  = Empty
  | Zombie
    { player    :: Player
    , direction :: Clockwise
    } deriving Eq
instance Show Zombie where
  show Empty        = "  "
  show (Zombie p d) = show p ++ show d

cyclicSucc :: forall a. (Enum a, Bounded a) => a -> a
cyclicSucc a | fromEnum a >= fromEnum (maxBound :: a) = minBound
             | otherwise                              = succ a

cyclicPred :: forall a. (Enum a, Bounded a) => a -> a
cyclicPred a | fromEnum a <= fromEnum (minBound :: a) = maxBound
             | otherwise                              = pred a

cyclicToEnum :: forall a. (Enum a, Bounded a) => Int -> a
cyclicToEnum i = toEnum $ i `mod` length ([minBound .. maxBound :: a])

modifyL :: Monad m
        => ([a] -> m [a]) -> (Int, Int) -> StateT (M.Matrix a) m ()
modifyL f (x, y) = do
  m  <- get
  let fixed = downer (M.nrows m) y
  ls <- lift $ f $ catMaybes $ map (\y' -> M.safeGet x y' m)
                             $ enumToFrom fixed 1
  put $ M.matrix (M.nrows m) (M.ncols m) $ \(r,c) ->
    maybe (M.unsafeGet r c m) id
    $ guard (r == x) >> ls `safeIx` (fixed - c)

modifyU :: Monad m
        => ([a] -> m [a]) -> (Int, Int) -> StateT (M.Matrix a) m ()
modifyU f (x, y) = do
  m  <- get
  let fixed = downer (M.ncols m) x
  ls <- lift $ f $ catMaybes $ map (\x' -> M.safeGet x' y m)
                             $ enumToFrom fixed 1
  put $ M.matrix (M.nrows m) (M.ncols m) $ \(r,c) ->
    maybe (M.unsafeGet r c m) id
    $ guard (c == y) >> ls `safeIx` (fixed - r)

modifyR :: Monad m
        => ([a] -> m [a]) -> (Int, Int) -> StateT (M.Matrix a) m ()
modifyR f (x, y) = do
  m  <- get
  let fixed = upper 1 y
  ls <- lift $ f $ catMaybes $ map (\y' -> M.safeGet x y' m)
                             $ enumFromTo fixed (M.nrows m)
  put $ M.matrix (M.nrows m) (M.ncols m) $ \(r,c) ->
    maybe (M.unsafeGet r c m) id
    $ guard (r == x) >> ls `safeIx` (c -fixed)

modifyD :: Monad m
        => ([a] -> m [a]) -> (Int, Int) -> StateT (M.Matrix a) m ()
modifyD f (x, y) = do
  m  <- get
  let fixed = upper 1 x
  ls <- lift $ f $ catMaybes $ map (\x' -> M.safeGet x' y m)
                             $ enumFromTo fixed (M.ncols m)
  put $ M.matrix (M.nrows m) (M.ncols m) $ \(r,c) ->
    maybe (M.unsafeGet r c m) id
    $ guard (c == y) >> ls `safeIx` (r -fixed)

modifyLURD :: Monad m
           => ([a] -> m [a]) -> (Int, Int) -> StateT (M.Matrix a) m ()
modifyLURD f xy = do
  modifyL f xy
  modifyU f xy
  modifyR f xy
  modifyD f xy

downer :: Ord a => a -> a -> a
downer a b = if a <= b then a else b

upper :: Ord a => a -> a -> a
upper a b = if a >= b then a else b

enumToFrom :: Enum a => a -> a -> [a]
enumToFrom a = enumFromThenTo a $ pred a

safeIx :: [a] -> Int -> Maybe a
safeIx ls i = guard (i >= 0) >> listToMaybe (drop i ls)

_4x4 :: M.Matrix Clockwise
_4x4 = M.fromList 4 4 $ iterate cyclicSucc L

f :: [Clockwise] -> IO [Clockwise]
f ls = do
  print ls
  return $ fmap cyclicSucc ls

printState :: (MonadIO m, Show r) => StateT r m ()
printState = get >>= liftIO . print

randomCol :: Int -> Player -> IO (M.Matrix Zombie)
randomCol r p = do
  gen <- R.createSystemRandom
  vec <- R.uniformVector gen r :: IO (V.Vector Int)
  return $ M.colVector
         $ (Zombie p . cyclicToEnum) <$> vec

fill :: a -> Int -> Int -> M.Matrix a
fill a r c = M.matrix r c $ \_ -> a

initialMatrix :: Int -> Int -> IO (M.Matrix Zombie)
initialMatrix r c = do
  let col = fill Empty (r - 2) 1
      row = fill Empty 1 c
  left  <- randomCol (r - 2) Blue
  right <- randomCol (r - 2) Red
  let gu = col M.<|> left
             M.<|> fill Empty (r - 2) (c - 4)
               M.<|> right M.<|> col
  return $ row M.<-> gu M.<-> row

main :: IO ()
main = flip evalStateT _4x4 $ do
  -- modifyLURD f (2,2)
  -- printState
  liftIO $ initialMatrix 6 6 >>= print
