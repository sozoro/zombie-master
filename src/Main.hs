-- {-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE ViewPatterns #-}

module Main where

import Color
import Cyclic
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable (asum)
import Data.Functor
import Data.List (sort,nub)
import Data.Maybe (listToMaybe,maybeToList,catMaybes,fromMaybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Control.Exception         as E
import qualified Data.Colour.SRGB          as C
import qualified Data.Matrix               as M
import qualified Data.Vector               as V
import qualified System.Console.ANSI       as A
import qualified System.Console.ANSI.Types as A
import qualified System.Random.MWC         as R

data Clockwise = L | U | R | D deriving (Eq, Enum, Bounded, Ord, Generic, R.Uniform)
instance Cyclic Clockwise where
instance Show   Clockwise where
  show L = "←"
  show U = "↑"
  show R = "→"
  show D = "↓"

instance R.Variate Clockwise where
  uniform        = fmap cyclicToEnum . R.uniformR (0, 4)
  uniformR (x,y) = fmap cyclicToEnum . R.uniformR (fromEnum x,fromEnum y)

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

fillL :: Int -> ColorStr -> ColorStr
fillL n colStr = replicate (n - length colStr) ' ' `addLeft` colStr

fillR :: Int -> ColorStr -> ColorStr
fillR n colStr = colStr ++ replicate (n - length colStr)
                 (ColorChar (V2 Through Through) ' ')

maxLength :: M.Matrix [a] -> Int
maxLength = foldr max 0 . fmap length

fillCMR :: Int -> M.Matrix ColorStr -> M.Matrix ColorStr
fillCMR n = fmap $ fillR n

fillCML :: Int -> M.Matrix ColorStr -> M.Matrix ColorStr
fillCML n = fmap $ fillL n

fillMaxCMR :: M.Matrix ColorStr -> M.Matrix ColorStr
fillMaxCMR m = fillCMR (maxLength m) m

fillMaxCML :: M.Matrix ColorStr -> M.Matrix ColorStr
fillMaxCML m = fillCML (maxLength m) m

addLFs :: M.Matrix ColorStr -> M.Matrix ColorStr
addLFs m = m M.<|> M.colVector (V.replicate (M.nrows m) [lineFeed])

frame :: M.Matrix ColorStr -> M.Matrix ColorStr
frame m = left M.<|> (horiBar M.<-> m M.<-> horiBar) M.<|> right
  where
    frameColor      = V2 (NewColor $ C.sRGB 0.419 0.776 1) Reset
    frameSpaceColor = V2 Reset                             Reset
    f c             = ColorChar frameColor      c
    s               = ColorChar frameSpaceColor ' '
    vertBar         = M.colVector $ V.replicate (M.nrows m) [s, f '│', s]
    horiBar         = M.rowVector $ V.replicate (M.ncols m)
                    $ replicate (maxLength m) $ f '─'
    cornerL c       = M.colVector $ V.singleton [s,     f c, f '─']
    cornerR c       = M.colVector $ V.singleton [f '─', f c, s    ]
    left            = cornerL '┌' M.<-> vertBar M.<-> cornerL '└'
    right           = cornerR '┐' M.<-> vertBar M.<-> cornerR '┘'

safeSubmatrix :: Int -> Int -> Int -> Int -> M.Matrix a -> Maybe (M.Matrix a)
safeSubmatrix r1' r2' c1' c2' m
  | r2' < r1' || c2' < c1' = Nothing
  | otherwise = Just $ M.submatrix r1 r2 c1 c2 m
  where
    rm = M.nrows m
    cm = M.ncols m
    r1 = range 1  rm r1'
    r2 = range r1 rm r2'
    c1 = range 1  cm c1'
    c2 = range c1 cm c2'
    range d u x | x < d = d | x > u = u | otherwise = x

type FourBlocks a = (a,a,a,a)

safeSplitBlocks :: Int -> Int -> M.Matrix a -> FourBlocks (Maybe (M.Matrix a))
safeSplitBlocks r c m =
  ( safeSubmatrix 1     r  1 c m , safeSubmatrix 1     r  (c+1) cm m
  , safeSubmatrix (r+1) rm 1 c m , safeSubmatrix (r+1) rm (c+1) cm m )
  where
    rm = M.nrows m
    cm = M.ncols m

joinableH :: M.Matrix a -> M.Matrix a -> Bool
joinableH m1 m2 = M.nrows m1 == M.nrows m2

joinableV :: M.Matrix a -> M.Matrix a -> Bool
joinableV m1 m2 = M.ncols m1 == M.ncols m2

joinMaybeH :: Maybe (M.Matrix a) -> Maybe (M.Matrix a) -> Maybe (M.Matrix a)
joinMaybeH mm1 mm2 =
  ((joinableH <$> mm1 <*> mm2 >>= guard) >> liftA2 (M.<|>) mm1 mm2)
  <|> mm1 <|> mm2

joinMaybeV :: Maybe (M.Matrix a) -> Maybe (M.Matrix a) -> Maybe (M.Matrix a)
joinMaybeV mm1 mm2 =
  ((joinableV <$> mm1 <*> mm2 >>= guard) >> liftA2 (M.<->) mm1 mm2)
  <|> mm1 <|> mm2

joinMaybesH :: [Maybe (M.Matrix a)] -> Maybe (M.Matrix a)
joinMaybesH []     = Nothing
joinMaybesH (x:xs) = foldr (\b f a -> a `joinMaybeH` f b) id xs x

joinMaybesV :: [Maybe (M.Matrix a)] -> Maybe (M.Matrix a)
joinMaybesV []     = Nothing
joinMaybesV (x:xs) = foldr (\b f a -> a `joinMaybeV` f b) id xs x

joinMaybeBlocks :: FourBlocks (Maybe (M.Matrix a)) -> Maybe (M.Matrix a)
joinMaybeBlocks (Just m1, Just m2, Just m3, Just m4) =
  Just $ M.joinBlocks (m1, m2, m3, m4)
joinMaybeBlocks (mm1,     mm2,     mm3,     mm4)     =
  (mm1 `joinMaybeH` mm2) `joinMaybeV` (mm3 `joinMaybeH` mm4)

insertLines :: FBChangeColor -> [Int] -> [Int] -> M.Matrix ColorStr
            -> M.Matrix ColorStr
insertLines lineColor rs cs m = fromMaybe (M.rowVector V.empty)
  $ joinMaybesV $ joinMaybesH <$> sms
  where
    ps n ls = foldr (\j f i -> (i, j) : f (succ j)) (\i -> [(i, n)]) ls 1
    rm  = M.nrows m
    cm  = M.ncols m
    rps = ps rm $ nub $ sort rs
    cps = ps cm $ nub $ sort cs
    sms = fmap (\(r1,r2) -> fmap (\(c1,c2) -> f r1 r2 c1 c2) cps) rps
    f r1 r2 c1 c2 = joinMaybeBlocks
      ( safeSubmatrix r1 r2 c1 c2 m
      , guard (c2 < cm || c1 > cm) >> vl
          (flip appHeadBack (succ c2) <$> [r1..r2])
      , guard (r2 < rm || r1 > rm) >> hl (succ $ c2 - c1)
      , guard ((c2 < cm || c1 > cm) && (r2 < rm || r1 > rm)) >> cr
      )
    mx = maxLength m
    th = lineColor { v2y = Through }
    cr = Just $ M.rowVector $ V.singleton $ monochroStrs [(lineColor, "─┼─")]
    ho = replicate mx $ ColorChar lineColor '─'
    ve c = [ColorChar th ' ', ColorChar lineColor '│', ColorChar c ' ']
    hl n | n <= 0    = Nothing
         | otherwise = Just $ M.rowVector $ V.replicate n ho
    vl [] = Nothing
    vl ls = Just $ M.colVector $ V.fromList $ ve <$> ls
    newColor (NewColor c) = Just c
    newColor _            = Nothing
    getHeadBack r c = do
      e <- M.safeGet r c m
      h <- listToMaybe e
      newColor $ v2y $ ccColor $ h
    appHeadBack r c =
      maybe lineColor (\c -> lineColor { v2y = NewColor c })
      $ getHeadBack r c

grid gridColor m = insertLines gridColor [0..M.nrows m] [0..M.ncols m] m

index :: FBChangeColor -> M.Matrix ColorStr -> M.Matrix ColorStr
index indexColor m = M.joinBlocks (z, horiIx, vertIx, m)
  where
    index str = monochroStrs [(indexColor, str)]
    vertIx = fmap (index . show) $ M.colVector $ V.enumFromTo 1 $ M.nrows m
    horiIx = fmap (index . show) $ M.rowVector $ V.enumFromTo 1 $ M.ncols m
    z      = M.rowVector $ V.singleton $ index ""

type Pos    = V2 Int
type PosVec = V2 Int

dirVec :: Clockwise -> PosVec
dirVec L = V2 0    (-1)
dirVec U = V2 (-1) 0 
dirVec R = V2 0    1 
dirVec D = V2 1    0 

v2Tuple :: V2 a -> (a, a)
v2Tuple (V2 x y) = (x, y)

tupleV2 :: (a, a) -> V2 a
tupleV2 = uncurry V2

modifyViaSubList :: Functor f => [Pos] -> ([a] -> f [a])
                 -> M.Matrix a -> f (M.Matrix a)
modifyViaSubList ps f m = fmap (foldr id m)
  $ fmap . zipWith (flip M.setElem) <*> f . fmap (m M.!)
  $ v2Tuple <$> ps

modifyTo :: Monad m => Clockwise -> Pos -> ([a] -> m [a])
         -> StateT (M.Matrix a) m ()
modifyTo d p f = StateT $ \m -> do
  let inRange (V2 r c) =  1 <= r && r <= M.nrows m
                       && 1 <= c && c <= M.ncols m
      ps = takeWhile inRange $ iterate (+ dirVec d) p
  ((),) <$> modifyViaSubList ps f m

allDirs :: Monad m => Pos -> ([a] -> m [a]) -> StateT (M.Matrix a) m ()
allDirs p f = forM_ [minBound..maxBound] $ \d -> modifyTo d p f

printState :: (MonadIO m, Show r) => StateT r m ()
printState = get >>= liftIO . print

randomCol :: MonadIO m => Int -> Player -> m (M.Matrix Zombie)
randomCol r p = do
  gen <- liftIO R.createSystemRandom
  vec <- liftIO $ R.uniformVector gen r
  return $ M.colVector $ Zombie p <$> vec

fill :: a -> Int -> Int -> M.Matrix a
fill a r c = M.matrix r c $ \_ -> a

data TooSmallMatrix = TooSmallMatrix deriving (Show, Typeable)
instance E.Exception TooSmallMatrix where

initialMatrix :: MonadIO m => Int -> Int -> m (M.Matrix Zombie)
initialMatrix r c = do
  when (r <= 2 || c < 4) $ liftIO $ E.throwIO TooSmallMatrix
  let col = fill Empty (r - 2) 1
      row = fill Empty 1 c
  left  <- randomCol (r - 2) Blue
  right <- randomCol (r - 2) Red
  let nl = sum $ fmap (fromEnum . (R ==) . direction) left
      nr = sum $ fmap (fromEnum . (L ==) . direction) right
  if (nl == 0 || nr == 0 || nl == nr) then initialMatrix r c
  else do
    let gu = col M.<|> left  M.<|> fill Empty (r - 2) (c - 4)
                 M.<|> right M.<|> col
    return $ row M.<-> gu    M.<-> row

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
            => Pos -> StateT Status m CellStatus
checkZombie pos = zombies <$> get >>= \m -> return $ checkZombie' m pos

checkZombie' :: M.Matrix Zombie -> Pos -> CellStatus
checkZombie' m pos@(V2 y x) =
  flip (maybe IsOutOfTheField) (M.safeGet y x m) $ \case
    Empty      -> IsEmpty
    Zombie p d -> IsZombie $
      let front = checkZombie' m $ pos + dirVec d in
      if  front == IsEmpty then Forwardable p d
                           else DeadLocked  p d

succPlayer :: Monad m => StateT Status m ()
succPlayer =
  modify $ \s@(Status {..}) -> s { nextPlayer = cyclicSucc nextPlayer }

advanceZombie :: MonadIO m => Pos -> StateT Status m ()
advanceZombie pos = checkZombie pos >>= \case
  IsZombie (Forwardable p d) -> do
         Status {..} <- get
         if p /= nextPlayer
         then liftIO $ print NotYourZombie
         else do
           let newPos = pos + dirVec d
           zombiesState $ modifyTo d pos $ \case
             (z:e:xs) -> return (e:z:xs)
             _        -> return []
           zombiesState $ rule newPos
           succPlayer
           modify $ \s -> s { previous = Just newPos }
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
     => Pos -> StateT (M.Matrix Zombie) m ()
rule pos = allDirs pos $ \case
  (a@(Zombie _ _):as) -> let (space, remaining) = break isZombie as
                             f b = if a `isEnemyOf` b then turnZombie b else b
                         in return $ a : (space ++ mapHead f remaining)
  _ -> return []

checkZombies :: Monad m => StateT Status m (M.Matrix CellStatus)
checkZombies = zombies <$> get >>= \m ->
  return $ M.matrix (M.nrows m) (M.ncols m) $ checkZombie' m . tupleV2

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

dontColor :: ColorSetter
dontColor _ _ = A.Reset

colorStrZombies :: Monad m => StateT Status m ColorStr
colorStrZombies = do
  Status {..} <- get
  return $ concat $ addLFs $ grid (V2 Reset Reset) $ fillMaxCML
         $ index (V2 Reset Reset)
         $ fromMaybe id
           (setBackColor (NewColor $ C.sRGB 0.2 0.2 0.2) <$> previous)
         $ fmap colorShow $ zombies

data Status = Status
  { zombies    :: !(M.Matrix Zombie)
  , nextPlayer :: !Player
  , previous   :: !(Maybe Pos)
  } deriving (Show,Eq)

zombiesState :: Monad m
             => StateT (M.Matrix Zombie) m a -> StateT Status m a
zombiesState f =
  StateT $ \s -> fmap (\zs -> s { zombies = zs }) <$> runStateT f (zombies s)


setBackColor :: ChangeColor -> Pos
             -> M.Matrix ColorStr -> M.Matrix ColorStr
setBackColor back p = M.mapPos $ \p' -> if tupleV2 p' == p
  then fmap $ \(ColorChar (V2 fore _) cha) -> ColorChar (V2 fore back) cha
  else id


zombieMaster :: IO ()
zombieMaster = withColor setColor24bit
  $ initialMatrix 8 8 >>= \m -> flip evalStateT (Status
  { zombies    = m
  , nextPlayer = Blue
  , previous   = Nothing
  }) $ forever $ do
    Status {..} <- get
    cs <- checkZombies
    zs <- colorStrZombies
    lift $ putColorStrLn zs
    if not $ actionable nextPlayer cs
    then if or $ forwardable <$> cs
      then liftIO (print $ Passed nextPlayer) >> succPlayer
      else liftIO $ E.throwIO Draw
    else do
      liftIO $ putStr "next player is: "
      liftIO $ print nextPlayer
      line <- liftIO $ getLine
      maybe (liftIO $ print WrongFormat) (advanceZombie . tupleV2)
        $ listToMaybe $ fmap fst $ (reads :: ReadS (Int, Int)) line


main :: IO ()
main = zombieMaster
