-- {-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Cyclic
import Color (test)
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
    [(Just $ V2 (Just $ toColor p) Nothing, show p)]

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
  colorShow Empty        = ColorStr [space]
  colorShow (Zombie p d) = colorShow p `addCS` monochroStrs [(through, show d)]

space :: ColorChar
space = ColorChar Nothing ' '

colorUnlines :: [ColorStr] -> ColorStr
colorUnlines = ColorStr . f . fmap colorChars
  where
    f []       = []
    f (xs:xss) = xs ++ (ColorChar Nothing '\n') : f xss

colorUnwords :: [ColorStr] -> ColorStr
colorUnwords = ColorStr . join . fmap colorChars

prettyColorMatrix :: ColorShow a
                  => ColoredOrNot -> ColoredOrNot
                  -> M.Matrix a -> ColorStr
prettyColorMatrix c1 c2 m = colorUnlines
  [ monochroStrs [(c1, "| ")] `addCS`
    (colorUnwords $ fmap (\c -> fillS mx $ colorShow $ m M.! (r,c))
     [1..M.ncols m])
  | r <- [1..M.nrows m] ]
  where
    mx = foldr max 0 $ fmap (length . colorChars . colorShow) m
    fillS k colStr =
      underCS (replicate (k - length (colorChars colStr)) space ++) colStr

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


data V2 a = V2 { v2x :: !a , v2y :: !a } deriving (Show, Eq)
instance Functor V2 where
  fmap f (V2 fore back) = V2 (f fore) (f back)
instance Applicative V2 where
  pure a            = V2 a     a
  V2 f g <*> V2 x y = V2 (f x) (g y)
instance Foldable V2 where
  foldr f z (V2 x y) = f x $ f y z
instance Semigroup a => Semigroup (V2 a) where
  V2 x1 y1 <> V2 x2 y2 = V2 (x1 <> x2) (y1 <> y2)
instance Monoid a => Monoid (V2 a) where
  mempty = V2 mempty mempty

fb :: V2 A.ConsoleLayer
fb = V2 A.Foreground A.Background

type Color        = C.Colour Float
type ColorDiff    = Maybe Color
type FBColorDiff  = V2 ColorDiff
type ColoredOrNot = Maybe FBColorDiff

data ColorChar = ColorChar
  { cwcColor :: !ColoredOrNot
  , cwcChar  :: !Char
  } deriving (Show, Eq)

through :: ColoredOrNot
through = Just $ V2 Nothing Nothing

newtype ColorStr = ColorStr { colorChars :: [ColorChar] }

underCS :: ([ColorChar] -> [ColorChar]) -> ColorStr -> ColorStr
underCS f (ColorStr ls) = ColorStr $ f ls

addCS :: ColorStr -> ColorStr -> ColorStr
addCS cs = underCS (colorChars cs ++)

class Monad m => MonadAddCharStr m where
  addChar :: Char   -> m ()
  addStr  :: String -> m ()

instance MonadAddCharStr IO where
  addChar = putChar
  addStr  = putStr

instance MonadAddCharStr (State ShowS) where
  addChar cha = modify $ \f -> f . (cha :)
  addStr  str = modify $ \f -> f . (str ++)

type ColorSetter = A.ConsoleLayer -> Color -> A.SGR
type WithColor m a = ReaderT ColorSetter m a

setColor24bit :: ColorSetter
setColor24bit = A.SetRGBColor

toSRGB6Level :: (RealFrac b, Floating b)
             => C.Colour b -> C.RGB (IsCyclic FinSix)
toSRGB6Level = C.toSRGBBounded

setColor6Level :: ColorSetter
setColor6Level layer color =
  A.SetPaletteColor layer $ A.xterm6LevelRGB r g b
  where
    C.RGB r g b = fromIntegral <$> toSRGB6Level color

colorChar :: MonadAddCharStr m
          => ColorChar -> StateT FBColorDiff (ReaderT ColorSetter m) ()
colorChar (ColorChar col cha) = do
  old <- get
  let new       = update (if cha == '\n' then Nothing else col) old
  let diffCol   = old `diff` new
  when (diffCol /= fmap (const Nothing) old) $ do
    let changings = asum $ maybeToList
                        <$> ((\a -> fmap (a,)) <$> fb <*> fmap join diffCol)
    setter <- lift ask
    lift $ lift $ addStr $ A.setSGRCode $ uncurry setter <$> changings
    put new
  lift $ lift $ addChar cha

update :: Alternative f => Maybe (V2 (f a)) -> V2 (f a) -> V2 (f a)
update Nothing   = const $ V2 empty empty
update (Just fb) = ((<|>) <$> fb <*>)

diff :: (Applicative f, Eq a) => f a -> f a -> f (Maybe a)
diff x = (diff' <$> x <*>)
  where
    diff' c d = if c == d then Nothing else Just d

colorStr :: MonadAddCharStr m => ColorStr -> WithColor m ()
colorStr = flip evalStateT (V2 Nothing Nothing)
         . mapM_ colorChar . colorChars

showsColorStr :: ColorSetter -> ColorStr -> ShowS
showsColorStr setter
  = flip execState id . flip runReaderT setter . colorStr

instance Show ColorStr where
  show = flip (showsColorStr setColor6Level) $ A.setSGRCode [A.Reset]

putColorStr :: ColorStr -> WithColor IO ()
putColorStr = colorStr

putColorStrLn :: ColorStr -> WithColor IO ()
putColorStrLn sc = do
  colorStr sc
  liftIO $ putStrLn $ A.setSGRCode [A.Reset]

withColor :: ColorSetter -> WithColor IO a -> IO a
withColor setter m = E.onException (runReaderT m setter)
                   $ putStrLn $ A.setSGRCode [A.Reset]

monochroStrs :: [(ColoredOrNot, String)] -> ColorStr
monochroStrs = ColorStr . join . fmap (uncurry $ fmap . ColorChar)

class ColorShow a where
  colorShow :: a -> ColorStr

{-
main :: IO ()
main = withColor setColor24bit $ do
  let hw = monochroStrs [ (fb1, "hello")
                        , (Nothing, " ")
                        , (fb2, "world")
                        ]
  let smooze = ColorStr $ take 5000 $ drop (256 ^ 3 `div` 2)
                        $ colorChars $ fullColor
  z <- prettyColorMatrix fb1 fb2 <$> liftIO (initialMatrix 10 10)
  -- liftIO $ print smooze
  putColorStrLn z
  putColorStrLn $ prettyColorMatrix fb1 fb2 $ fill (Zombie Red D) 10 10
  where
    color1 = Just $ C.sRGB 0.419 0.776 1
    color2 = Just $ C.sRGB 0.678 0.019 0.274
    fb1 = Just $ V2 color1 color2
    fb2 = Just $ V2 color2 color1
-}

main = test

fullColor :: ColorStr
fullColor = ColorStr
  $ (flip ColorChar ' ' . Just . V2 Nothing . Just . uncurry3 C.sRGB24)
  <$> [ (r, g, b) | r <- [0..255], g <- [0..255], b <- [0..255] ]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
