{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, AllowAmbiguousTypes #-}
module Weights where 

data Kg
data Lb
data Pd

class WeightUnit a where

    unitname :: String

    kgFactor :: Float


instance WeightUnit Kg where

    unitname = "kg"
    kgFactor = 1

instance WeightUnit Lb where

    unitname = "lb"
    kgFactor = 0.453592

instance WeightUnit Pd where

    unitname = "pd"
    kgFactor = 16.3807


newtype Weight unit = Weight Float
  deriving (Num, Fractional)

class Convert a b where

    convert :: a -> b

instance (WeightUnit a, WeightUnit b) => Convert (Weight a) (Weight b) where

    convert (Weight x) = Weight $ x * (kgFactor @a) / (kgFactor @b)

instance (WeightUnit a) => Show (Weight a) where

    show (Weight x) = show x ++ (unitname @a)

add :: (WeightUnit a, WeightUnit b) => Weight a -> Weight b -> Weight a
add x y = x + (convert y)

asKg :: Float -> Weight Kg
asKg = Weight

asLb :: Float -> Weight Lb
asLb = Weight

asPd :: Float -> Weight Pd
asPd = Weight
