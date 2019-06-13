{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Cyclic
  ( Cyclic (..)
  , IsCyclic (..)
  , isCyclic
  , fromInt
  , FinOne
  , FinTwo
  , FinThree
  , FinFour
  , FinFive
  , FinSix
  , FinSeven
  , FinEight
  , FinNine
  , FinTen
  ) where

class (Enum a, Bounded a) => Cyclic a where
  cyclicSucc :: a -> a
  cyclicSucc a | fromEnum a >= fromEnum (maxBound :: a) = minBound
               | otherwise                              = succ a

  cyclicPred :: a -> a
  cyclicPred a | fromEnum a <= fromEnum (minBound :: a) = maxBound
               | otherwise                              = pred a

  cyclicToEnum :: Int -> a
  cyclicToEnum i = toEnum $ i `mod` length ([minBound .. maxBound :: a])

data IsCyclic a where
  IsCyclic :: Cyclic a => a -> IsCyclic a

isCyclic :: IsCyclic a -> a
isCyclic (IsCyclic a) = a

instance Eq a => Eq (IsCyclic a) where
  (IsCyclic a) == (IsCyclic b) = a == b
  (IsCyclic a) /= (IsCyclic b) = a /= b

instance Ord a => Ord (IsCyclic a) where
  (IsCyclic a) <  (IsCyclic b)      = a <  b
  (IsCyclic a) <= (IsCyclic b)      = a <= b
  (IsCyclic a) >  (IsCyclic b)      = a >  b
  (IsCyclic a) >= (IsCyclic b)      = a >= b
  compare (IsCyclic a) (IsCyclic b) = compare a b
  max     (IsCyclic a) (IsCyclic b) = IsCyclic $ max a b
  min     (IsCyclic a) (IsCyclic b) = IsCyclic $ min a b

instance Show a => Show (IsCyclic a) where
  show = show . isCyclic

instance Cyclic a => Bounded (IsCyclic a) where
  minBound = IsCyclic $ minBound
  maxBound = IsCyclic $ maxBound

instance Cyclic a => Enum (IsCyclic a) where
  toEnum    = IsCyclic . toEnum
  fromEnum  = fromEnum . isCyclic
  succ      = IsCyclic . succ . isCyclic
  pred      = IsCyclic . pred . isCyclic
  enumFrom  = map IsCyclic . enumFrom . isCyclic
  enumFromThen (IsCyclic a) (IsCyclic b) = map IsCyclic $ enumFromThen a b
  enumFromTo   (IsCyclic a) (IsCyclic b) = map IsCyclic $ enumFromTo   a b
  enumFromThenTo (IsCyclic a) (IsCyclic b) (IsCyclic c)
    = map IsCyclic $ enumFromThenTo a b c

fromInt :: Cyclic a => Int -> IsCyclic a
fromInt = IsCyclic . cyclicToEnum

instance Cyclic a => Num (IsCyclic a) where
  IsCyclic a + IsCyclic b = fromInt $ fromEnum a + fromEnum b
  IsCyclic a - IsCyclic b = fromInt $ fromEnum a - fromEnum b
  IsCyclic a * IsCyclic b = fromInt $ fromEnum a * fromEnum b
  abs    (IsCyclic a)     = fromInt $ abs    $ fromEnum a
  signum (IsCyclic a)     = fromInt $ signum $ fromEnum a
  fromInteger             = fromInt . fromInteger

instance (Cyclic a, Ord a) => Real (IsCyclic a) where
  toRational = toRational . fromEnum . isCyclic

instance (Cyclic a, Ord a) => Integral (IsCyclic a) where
  toInteger (IsCyclic a)              = toInteger $ fromEnum a
  quotRem   (IsCyclic a) (IsCyclic b) = (fromInt q, fromInt r)
    where (q, r) = quotRem (fromEnum a) (fromEnum b)

data FinOne
  = FinOne0
  deriving (Show, Eq, Ord, Bounded, Enum)
instance Cyclic FinOne where

data FinTwo
  = FinTwo0
  | FinTwo1
  deriving (Show, Eq, Ord, Bounded, Enum)
instance Cyclic FinTwo where

data FinThree
  = FinThree0
  | FinThree1
  | FinThree2
  deriving (Show, Eq, Ord, Bounded, Enum)
instance Cyclic FinThree where

data FinFour
  = FinFour0
  | FinFour1
  | FinFour2
  | FinFour3
  deriving (Show, Eq, Ord, Bounded, Enum)
instance Cyclic FinFour where

data FinFive
  = FinFive0
  | FinFive1
  | FinFive2
  | FinFive3
  | FinFive4
  deriving (Show, Eq, Ord, Bounded, Enum)
instance Cyclic FinFive where

data FinSix
  = FinSix0
  | FinSix1
  | FinSix2
  | FinSix3
  | FinSix4
  | FinSix5
  deriving (Show, Eq, Ord, Bounded, Enum)
instance Cyclic FinSix where

data FinSeven
  = FinSeven0
  | FinSeven1
  | FinSeven2
  | FinSeven3
  | FinSeven4
  | FinSeven5
  | FinSeven6
  deriving (Show, Eq, Ord, Bounded, Enum)
instance Cyclic FinSeven where

data FinEight
  = FinEight0
  | FinEight1
  | FinEight2
  | FinEight3
  | FinEight4
  | FinEight5
  | FinEight6
  | FinEight7
  deriving (Show, Eq, Ord, Bounded, Enum)
instance Cyclic FinEight where

data FinNine
  = FinNine0
  | FinNine1
  | FinNine2
  | FinNine3
  | FinNine4
  | FinNine5
  | FinNine6
  | FinNine7
  | FinNine8
  deriving (Show, Eq, Ord, Bounded, Enum)
instance Cyclic FinNine where

data FinTen
  = FinTen0
  | FinTen1
  | FinTen2
  | FinTen3
  | FinTen4
  | FinTen5
  | FinTen6
  | FinTen7
  | FinTen8
  | FinTen9
  deriving (Show, Eq, Ord, Bounded, Enum)
instance Cyclic FinTen where
