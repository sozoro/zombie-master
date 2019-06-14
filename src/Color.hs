{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Color
  ( V2 (..)

  , Color
  , ChangeColor (..)
  , FBColorDiff
  , FBChangeColor
  , through
  , reset

  , ColorChar (..)
  , ColorStr
  , ColorShow (..)
  , space
  , lineFeed
  , colorUnlines
  , monochroStrs
  , addLeft

  , MonadAddCharStr (..)
  , colorChar
  , colorStr

  , WithColor
  , ColorSetter
  , setColor24bit
  , setColor6Level

  , ColorStrShowS (..)
  , csShowS

  , ColorStrLazy (..)
  , csLazy

  , putColorStr
  , putColorStrLn
  , withColor
  ) where

import Cyclic
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Lazy
import Data.Foldable (asum,elem)
import Data.Maybe    (maybeToList)
import qualified Control.Exception    as E
import qualified Data.Colour.SRGB     as C
import qualified Data.Colour.RGBSpace as C
import qualified System.Console.ANSI  as A

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

type Color     = C.Colour Float
type ColorDiff = Maybe Color

data ChangeColor
  = Reset
  | Through
  | NewColor { newColor :: !Color }
  deriving (Show, Eq)

update :: ChangeColor -> ColorDiff -> ColorDiff
update Reset        = const Nothing
update Through      = id
update (NewColor c) = const $ Just c

type FBColorDiff   = V2 ColorDiff
type FBChangeColor = V2 ChangeColor

through :: FBChangeColor
through = V2 Through Through  

reset :: FBChangeColor
reset = V2 Reset Reset

data ColorChar = ColorChar
  { ccColor :: !FBChangeColor
  , ccChar  :: !Char
  } deriving (Show, Eq)

type ColorStr = [ColorChar]

space :: ColorChar
space = ColorChar reset ' '

lineFeed :: ColorChar
lineFeed = ColorChar reset '\n'

colorUnlines :: [ColorStr] -> ColorStr
colorUnlines = concatMap (++ [lineFeed])

monochroStrs :: [(FBChangeColor, String)] -> ColorStr
monochroStrs = (uncurry (fmap . ColorChar) =<<)

addLeft :: String -> ColorStr -> ColorStr
addLeft str []         = map (ColorChar reset)        str
addLeft str ccs@(cc:_) = map (ColorChar $ ccColor cc) str ++ ccs

class ColorShow a where
  colorShow :: a -> ColorStr

class Monad m => MonadAddCharStr m where
  addChar :: Char   -> m ()
  addStr  :: String -> m ()

instance MonadAddCharStr IO where
  addChar = putChar
  addStr  = putStr

instance MonadAddCharStr (State ShowS) where
  addChar cha = modify $ \f -> f . (cha :)
  addStr  str = modify $ \f -> f . (str ++)

instance MonadAddCharStr (State String) where
  addChar cha = modify (++ [cha])
  addStr  str = modify (++ str)

instance MonadAddCharStr (Writer String) where
  addChar = tell . (:[])
  addStr  = tell

type ColorSetter = A.ConsoleLayer -> Color -> A.SGR
type WithColor m a = ReaderT ColorSetter m a

setColor24bit :: ColorSetter
setColor24bit = A.SetRGBColor

toSRGB6Level :: (RealFrac b, Floating b)
             => C.Colour b -> C.RGB (IsCyclic FinSix)
toSRGB6Level = C.toSRGBBounded

setColor6Level :: ColorSetter
setColor6Level layer color
  = A.SetPaletteColor layer $ C.uncurryRGB A.xterm6LevelRGB
  $ fromIntegral <$> toSRGB6Level color

diff :: (Applicative f, Eq a) => f a -> f a -> f (Maybe a)
diff x = (diff' <$> x <*>)
  where
    diff' c d = if c == d then Nothing else Just d

colorChar :: MonadAddCharStr m
          => ColorChar -> StateT FBColorDiff (ReaderT ColorSetter m) ()
colorChar (ColorChar col cha) = do
  old <- get
  let new = update <$> (if cha == '\n' then reset else col) <*> old
      dif = old `diff` new
  when (Just Nothing `elem` dif)
    $ lift $ lift $ addStr $ A.setSGRCode [A.Reset]
  setter <- lift ask
  let fb        = V2 A.Foreground A.Background
      changings = asum $ maybeToList
                      <$> ((\a -> fmap (a,)) <$> fb <*> fmap join dif)
  unless (null changings)
    $ lift $ lift $ addStr $ A.setSGRCode $ uncurry setter <$> changings
  lift $ lift $ addChar cha
  put new

colorStr :: MonadAddCharStr m => ColorStr -> WithColor m ()
colorStr = flip evalStateT (V2 Nothing Nothing) . mapM_ colorChar

newtype ColorStrShowS = CSShowS { unCSShowS :: ColorStr }

csShowS :: ColorSetter -> ColorStrShowS -> ShowS
csShowS setter
  = flip execState id . flip runReaderT setter . colorStr . unCSShowS

instance Show ColorStrShowS where
  show = flip (csShowS setColor6Level) $ A.setSGRCode [A.Reset]

newtype ColorStrLazy = CSLazy { unCSLazy :: ColorStr }

csLazy :: ColorSetter -> ColorStrLazy -> String
csLazy setter = execWriter . flip runReaderT setter . colorStr . unCSLazy

instance Show ColorStrLazy where
  show = (++ A.setSGRCode [A.Reset] ++ "\n") . csLazy setColor6Level

putColorStr :: ColorStr -> WithColor IO ()
putColorStr = colorStr

putColorStrLn :: ColorStr -> WithColor IO ()
putColorStrLn sc = do
  colorStr sc
  liftIO $ putStrLn $ A.setSGRCode [A.Reset]

withColor :: ColorSetter -> WithColor IO a -> IO a
withColor setter m = E.onException (runReaderT m setter)
                   $ putStrLn $ A.setSGRCode [A.Reset]

_24bitFullColorStream :: ColorStr
_24bitFullColorStream = flip ColorChar ' ' . V2 Through . NewColor
  <$> [ C.sRGB24 r g b | r <- [0..255], g <- [0..255], b <- [0..255] ]
