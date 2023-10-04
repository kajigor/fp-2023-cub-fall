{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, AllowAmbiguousTypes #-}

module UnitsOfMeasure where

newtype Distance unit = Distance Double
  deriving (Num, Fractional)

data M
data Ft

getValue :: Distance unit -> Double
getValue (Distance v) = v

marathonInMeters :: Distance M
marathonInMeters = 42195

marathonInFeet :: Distance Ft
marathonInFeet = 138435

class Convert a b where
  convert :: a -> b

instance Convert (Distance M) (Distance Ft) where
  convert (Distance m) = Distance $ m *  3.28084

instance Convert (Distance Ft) (Distance M) where
  convert (Distance ft) = Distance $ ft * 0.3048

instance Convert a a where
  convert = id

-- marathonInMeters === marathonInFeet
--- === -- checks for equality given that the values' units can be converted to each other

class UnitName unit where
  unitName :: String

instance UnitName M where
  unitName = "m"

instance UnitName Ft where
  unitName = "ft"

instance UnitName unit => Show (Distance unit) where
  show (Distance d) = show d ++ unitName @unit
