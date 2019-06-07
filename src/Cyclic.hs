{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

import Data.Coerce

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
instance Eq a => Eq (IsCyclic a) where
  (IsCyclic a) == (IsCyclic b) = a == b
instance Ord a => Ord (IsCyclic a) where
  compare (IsCyclic a) (IsCyclic b) = compare a b
instance Show a => Show (IsCyclic a) where
  show (IsCyclic a) = show a
instance Cyclic a => Bounded (IsCyclic a) where
  minBound = IsCyclic $ minBound
  maxBound = IsCyclic $ maxBound
instance Cyclic a => Enum (IsCyclic a) where
  toEnum = IsCyclic . toEnum
  fromEnum (IsCyclic a) = fromEnum a

instance Cyclic a => Num (IsCyclic a) where
  IsCyclic x + IsCyclic y = IsCyclic $ cyclicToEnum $ fromEnum x + fromEnum y
  IsCyclic x - IsCyclic y = IsCyclic $ cyclicToEnum $ fromEnum x - fromEnum y
  IsCyclic x * IsCyclic y = IsCyclic $ cyclicToEnum $ fromEnum x * fromEnum y
  abs    (IsCyclic x)     = IsCyclic $ cyclicToEnum $ abs    $ fromEnum x
  signum (IsCyclic x)     = IsCyclic $ cyclicToEnum $ signum $ fromEnum x
  fromInteger             = IsCyclic . cyclicToEnum . fromInteger

instance (Cyclic a, Ord a) => Real (IsCyclic a) where
  toRational (IsCyclic a) = toRational $ fromEnum a

instance (Cyclic a, Ord a) => Integral (IsCyclic a) where
  quotRem (IsCyclic a) (IsCyclic b) = (IsCyclic q, IsCyclic r)
    where (q, r) = quotRem a b
  

main :: IO ()
main = return ()
