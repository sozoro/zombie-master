-- {-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Color
import Cyclic
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable (asum)
import Data.Maybe (listToMaybe,maybeToList,catMaybes)
import Data.Typeable (Typeable)
import qualified Control.Exception         as E
import qualified Data.Colour.SRGB          as C
import qualified Data.Matrix               as M
import qualified Data.Vector               as V
import qualified System.Console.ANSI       as A
import qualified System.Console.ANSI.Types as A
import qualified System.Random.MWC         as R

data Clockwise = L | U | R | D deriving (Eq, Enum, Bounded, Ord)
instance Cyclic Clockwise where
instance Show   Clockwise where
  -- show L = "⬅"
  -- show U = "⬆"
  -- show R = "➡"
  -- show D = "⬇"
  show L = "←"
  show U = "↑"
  show R = "→"
  show D = "↓"

data Player = Blue | Red deriving (Eq, Enum, Bounded, Ord)
instance Cyclic Player where
instance Show   Player where
  show Blue = "B"
  show Red  = "R"

toColor :: Player -> Color
toColor Blue = C.sRGB24 0   171 255
toColor Red  = C.sRGB24 255 0   171

instance ColorShow Player where
  colorShow p = monochroStrs
    [(V2 (NewColor $ toColor p) Reset, show p)]

data Zombie
  = Empty
  | Zombie
    { player    :: Player
    , direction :: Clockwise
    } deriving Eq
instance Show Zombie where
  show Empty        = "  "
  show (Zombie p d) = show p ++ show d
instance ColorShow Zombie where
  colorShow Empty        = [space]
  colorShow (Zombie p d) = colorShow p ++ monochroStrs [(through, show d)]

prettyColorMatrix :: ColorShow a
                  => FBChangeColor -> FBChangeColor
                  -> M.Matrix a -> ColorStr
prettyColorMatrix c1 c2 m = colorUnlines $
     [ replicate (rowMax + 2) ' ' `addLeft` concatMap
       (\i -> fillR mx $ monochroStrs [(c2, show i)]) [1..M.ncols m] ]
  ++ [ corner '┌' '┐' ]
  ++ [ fillL rowMax (monochroStrs [(c2, show r)])
       ++ monochroStrs [(c1, "│ ")]
       ++ (concat $ fmap (\c -> fillR mx $ colorShow $ m M.! (r,c))
            [1..M.ncols m])
       ++ monochroStrs [(c1, " │")]
     | r <- [1..M.nrows m] ]
  ++ [ corner '└' '┘' ]
  where
    rowMax         = length $ show $ M.nrows m
    mx             = (foldr max 0 $ fmap (length . colorShow) m)
                       `max` succ (length $ show $ M.ncols m)
    fillL k colStr = foldr addLeft colStr $ replicate (k - length colStr) " "
    fillR k colStr = colStr ++ replicate (k - length colStr)
                     (ColorChar (V2 Through Through) ' ')
    corner l r     = fillL rowMax []
                   ++ [ColorChar c1 l]
                   ++ replicate (M.ncols m * mx + 2) (ColorChar c1 '─')
                   ++ [ColorChar c1 r]

modifyL :: Monad m
        => Int -> Int -> ([a] -> m [a]) -> StateT (M.Matrix a) m ()
modifyL x y f = do
  m  <- get
  let fixed = downer (M.nrows m) y
  ls <- lift $ f $ catMaybes $ map (\y' -> M.safeGet x y' m)
                             $ enumToFrom fixed 1
  put $ M.matrix (M.nrows m) (M.ncols m) $ \(r,c) ->
    maybe (M.unsafeGet r c m) id
    $ guard (r == x) >> ls `safeIx` (fixed - c)

modifyU :: Monad m
        => Int -> Int -> ([a] -> m [a]) -> StateT (M.Matrix a) m ()
modifyU x y f = do
  m  <- get
  let fixed = downer (M.ncols m) x
  ls <- lift $ f $ catMaybes $ map (\x' -> M.safeGet x' y m)
                             $ enumToFrom fixed 1
  put $ M.matrix (M.nrows m) (M.ncols m) $ \(r,c) ->
    maybe (M.unsafeGet r c m) id
    $ guard (c == y) >> ls `safeIx` (fixed - r)

modifyR :: Monad m
        => Int -> Int -> ([a] -> m [a]) -> StateT (M.Matrix a) m ()
modifyR x y f = do
  m  <- get
  let fixed = upper 1 y
  ls <- lift $ f $ catMaybes $ map (\y' -> M.safeGet x y' m)
                             $ enumFromTo fixed (M.nrows m)
  put $ M.matrix (M.nrows m) (M.ncols m) $ \(r,c) ->
    maybe (M.unsafeGet r c m) id
    $ guard (r == x) >> ls `safeIx` (c - fixed)

modifyD :: Monad m
        => Int -> Int -> ([a] -> m [a]) -> StateT (M.Matrix a) m ()
modifyD x y f = do
  m  <- get
  let fixed = upper 1 x
  ls <- lift $ f $ catMaybes $ map (\x' -> M.safeGet x' y m)
                             $ enumFromTo fixed (M.ncols m)
  put $ M.matrix (M.nrows m) (M.ncols m) $ \(r,c) ->
    maybe (M.unsafeGet r c m) id
    $ guard (c == y) >> ls `safeIx` (r - fixed)

modifyLURD :: Monad m
           => Int -> Int -> ([a] -> m [a]) -> StateT (M.Matrix a) m ()
modifyLURD y x f = do
  modifyL y x f
  modifyU y x f
  modifyR y x f
  modifyD y x f

modifyTo :: Monad m
         => Clockwise -> Int -> Int -> ([a] -> m [a])
         -> StateT (M.Matrix a) m ()
modifyTo L = modifyL
modifyTo U = modifyU
modifyTo R = modifyR
modifyTo D = modifyD

downer :: Ord a => a -> a -> a
downer a b = if a <= b then a else b

upper :: Ord a => a -> a -> a
upper a b = if a >= b then a else b

enumToFrom :: Enum a => a -> a -> [a]
enumToFrom a = enumFromThenTo a $ pred a

safeIx :: [a] -> Int -> Maybe a
safeIx ls i = guard (i >= 0) >> listToMaybe (drop i ls)

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

data TooSmallMatrix = TooSmallMatrix deriving (Show, Typeable)
instance E.Exception TooSmallMatrix where

initialMatrix :: Int -> Int -> IO (M.Matrix Zombie)
initialMatrix r c = do
  when (r <= 2 || c < 4) $ E.throwIO TooSmallMatrix
  let col = fill Empty (r - 2) 1
      row = fill Empty 1 c
  left  <- randomCol (r - 2) Blue
  right <- randomCol (r - 2) Red
  let gu = col M.<|> left  M.<|> fill Empty (r - 2) (c - 4)
               M.<|> right M.<|> col
  return $ row M.<-> gu    M.<-> row

forward :: (Int, Int) -> Clockwise -> (Int, Int)
forward (y, x) L = (     y, pred x)
forward (y, x) U = (pred y,      x)
forward (y, x) R = (     y, succ x)
forward (y, x) D = (succ y,      x)

data CellStatus
  = IsOutOfTheField
  | IsEmpty
  | IsZombie ZombieStatus
  deriving (Typeable, Show, Eq)
instance E.Exception CellStatus where

data ZombieStatus
  = Forwardable Player Clockwise
  | DeadLocked  Player Clockwise
  deriving (Typeable, Show, Eq)
instance E.Exception ZombieStatus where

checkZombie :: Monad m
            => Int -> Int -> StateT (M.Matrix Zombie) m CellStatus
checkZombie y x = get >>= \m -> return $ checkZombie' m y x

checkZombie' :: M.Matrix Zombie -> Int -> Int -> CellStatus
checkZombie' m y x =
  flip (maybe IsOutOfTheField) (M.safeGet y x m) $ \case
    Empty      -> IsEmpty
    Zombie p d -> IsZombie $
      let front = uncurry (checkZombie' m) $ (y, x) `forward` d in
      if front == IsEmpty then Forwardable p d
                          else DeadLocked  p d

advanceZombie :: MonadIO m
              => Int -> Int
              -> StateT (M.Matrix Zombie) (StateT Player m) ()
advanceZombie y x = checkZombie y x >>= \case
  IsZombie (Forwardable p d) -> lift get >>= \me ->
         if p /= me
         then liftIO $ print NotYourZombie
         else do
           modifyTo d y x $ \case
             (z:e:xs) -> return (e:z:xs)
             _        -> return []
           uncurry rule $ (y, x) `forward` d
           lift $ modify cyclicSucc
  err -> liftIO $ print err

isZombie :: Zombie -> Bool
isZombie (Zombie _ _) = True
isZombie _            = False

isEnemyOf :: Zombie -> Zombie -> Bool
isEnemyOf (Zombie p _) (Zombie p' _) = p /= p'
isEnemyOf _            _             = False

turnZombie :: Zombie -> Zombie
turnZombie (Zombie p d) = Zombie p (cyclicSucc d)
turnZombie z            = z

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ []     = []
mapHead f (x:xs) = f x : xs

rule :: Monad m
     => Int -> Int -> StateT (M.Matrix Zombie) m ()
rule y x = modifyLURD y x $ \case
  (a@(Zombie _ _):as) -> let (space, remaining) = break isZombie as
                             f b = if a `isEnemyOf` b then turnZombie b else b
                         in return $ a : (space ++ mapHead f remaining)
  _ -> return []

checkZombies :: Monad m => StateT (M.Matrix Zombie) m (M.Matrix CellStatus)
checkZombies = get >>= \m ->
  return $ M.matrix (M.nrows m) (M.ncols m) $ \(r,c) -> checkZombie' m r c

forwardable :: CellStatus -> Bool
forwardable (IsZombie (Forwardable _ _)) = True
forwardable _                            = False

actionable :: Player -> M.Matrix CellStatus -> Bool
actionable p csmx = foldr f False csmx
  where
    f (IsZombie (Forwardable p' _)) = if p' == p then const True else id
    f _                             = id

data Message
  = Draw
  | Passed Player
  | WrongFormat
  | NotYourZombie
  deriving (Eq, Typeable)
instance E.Exception Message where
instance Show Message where
  show Draw          = "--- draw ---"
  show (Passed p)    = "--- player " ++ show p ++ " has passed ---"
  show WrongFormat   = "please enter in the collect format: (y, x)"
  show NotYourZombie = "it is not your zombie"

monoColor :: IO ()
monoColor = initialMatrix 10 10 >>= \m ->
  flip evalStateT Blue $ flip evalStateT m $ forever $ do
    printState
    cs <- checkZombies
    pl <- lift $ get
    if not $ actionable pl cs
    then if fmap forwardable cs == fmap (const False) cs
      then liftIO $ E.throwIO Draw
      else do liftIO $ print $ Passed pl
              lift $ modify cyclicSucc
    else do
      liftIO $ putStr "next player is: "
      lift $ printState
      line <- liftIO $ getLine
      maybe (liftIO $ print WrongFormat) (uncurry advanceZombie)
        $ listToMaybe $ fmap fst $ (reads :: ReadS (Int, Int)) line


main :: IO ()
main = withColor setColor24bit $ do
  let hw = monochroStrs [ (fb1,   "hello")
                        , (reset, " ")
                        , (fb2,   "world")
                        ]
  z <- prettyColorMatrix (V2 color1 Reset) (V2 color2 Reset)
       <$> liftIO (initialMatrix 10 10)
  putColorStrLn hw
  putColorStrLn z
  where
    color1 = NewColor $ C.sRGB 0.419 0.776 1
    color2 = NewColor $ C.sRGB 0.678 0.019 0.274
    fb1 = V2 color1 color2
    fb2 = V2 color2 color1
