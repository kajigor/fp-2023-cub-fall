{-# LANGUAGE DataKinds, KindSignatures, GeneralizedNewtypeDeriving #-}

module FixedUnits where

data Units = M | Ft

newtype Distance (unit :: Units) = Distance Double
  deriving (Num, Fractional, Show)

marathonInMeters :: Distance M
marathonInMeters = 42195

marathonInFeet :: Distance Ft
marathonInFeet = 138435