{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Color where

import Cyclic
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable (asum,elem)
import Data.Maybe (listToMaybe,maybeToList,catMaybes)
import qualified Control.Exception         as E
import qualified Data.Colour.SRGB          as C
import qualified System.Console.ANSI       as A
import qualified System.Console.ANSI.Types as A

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

hasReset :: Foldable f => f ChangeColor -> Bool
hasReset = foldr f False
  where
    f Reset = const True
    f _     = id

type FBColorDiff   = V2 ColorDiff
type FBChangeColor = V2 ChangeColor

through :: FBChangeColor
through = V2 Through Through  

reset :: FBChangeColor
reset = V2 Reset Reset

data ColorChar = ColorChar
  { cwcColor :: !FBChangeColor
  , cwcChar  :: !Char
  } deriving (Show, Eq)

newtype ColorStr = ColorStr { colorChars :: [ColorChar] }

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

diff :: (Applicative f, Eq a) => f a -> f a -> f (Maybe a)
diff x = (diff' <$> x <*>)
  where
    diff' c d = if c == d then Nothing else Just d

colorChar :: MonadAddCharStr m
          => ColorChar -> StateT FBColorDiff (ReaderT ColorSetter m) ()
colorChar (ColorChar col cha) = do
  old <- get
  let new = update <$> col <*> old
      dif = old `diff` new
  when (dif /= fmap (const Nothing) old) $ do
    when (Just Nothing `elem` dif)
      $ lift $ lift $ addStr $ A.setSGRCode [A.Reset]
    let changings = asum $ maybeToList
                        <$> ((\a -> fmap (a,)) <$> fb <*> fmap join dif)
    setter <- lift ask
    when (not $ null changings)
      $ lift $ lift $ addStr $ A.setSGRCode $ uncurry setter <$> changings
    put new
  lift $ lift $ addChar cha

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

monochroStrs :: [(FBChangeColor, String)] -> ColorStr
monochroStrs = ColorStr . join . fmap (uncurry $ fmap . ColorChar)

class ColorShow a where
  colorShow :: a -> ColorStr

_24bitFullColorStream :: ColorStr
_24bitFullColorStream = ColorStr
  $ (flip ColorChar ' ' . V2 Through . NewColor)
  <$> [ C.sRGB24 r g b | r <- [0..255], g <- [0..255], b <- [0..255] ]
