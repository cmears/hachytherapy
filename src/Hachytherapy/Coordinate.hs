{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}

module Hachytherapy.Coordinate
    (
      Coordinate
    , Ordinate
    , makeCoordinate
    , makeOrdinate
    , ordinateToMillimetre
    , distanceCoord
    , distanceCoordSquared
    , xord, yord, zord
    , coordinate
    , boundingBox
    )
  where

import Control.Lens
import Data.Serialize
import GHC.Generics

-- Coordinate in 3D space, in millimetres.
-- type Coordinate = (Double,Double,Double)

type Ordinate = Int

-- Each int is an ordinate in micrometres.
newtype Coordinate = Coordinate { unCoordinate :: (Ordinate, Ordinate, Ordinate) }
  deriving (Ord, Eq, Show, Read, Generic)

instance Serialize Coordinate

makeCoordinate :: (Double,Double,Double) -> Coordinate
makeCoordinate (x,y,z) = Coordinate (makeOrdinate x, makeOrdinate y, makeOrdinate z)

makeOrdinate :: Double -> Ordinate
makeOrdinate x = round (x * 1000)

ordinateToMillimetre :: Ordinate -> Double
ordinateToMillimetre x = fromIntegral x / 1000

-- Result is in centimetres.
distanceCoord :: Coordinate -> Coordinate -> Double
distanceCoord (Coordinate (x1,y1,z1)) (Coordinate (x2,y2,z2)) =
  let sq x = x*x
      innerterm = sq (fromIntegral (x1-x2)/10000)
                + sq (fromIntegral (y1-y2)/10000)
                + sq (fromIntegral (z1-z2)/10000)
      fullterm = {-# SCC "sqrt" #-} (sqrt innerterm)
  in fullterm

distanceCoordSquared :: Coordinate -> Coordinate -> Double
distanceCoordSquared (Coordinate (x1,y1,z1)) (Coordinate (x2,y2,z2)) =
  let sq x = x*x
      innerterm = sq (fromIntegral (x1-x2)/10000)
                + sq (fromIntegral (y1-y2)/10000)
                + sq (fromIntegral (z1-z2)/10000)
  in innerterm

coordinate :: Iso' Coordinate (Ordinate, Ordinate, Ordinate)
coordinate = iso unCoordinate Coordinate
xord, yord, zord :: Lens' Coordinate Ordinate
xord = coordinate . _1
yord = coordinate . _2
zord = coordinate . _3


-- Find the bounding box of some coordinates, and return the two
-- opposite corners.
boundingBox :: [Coordinate] -> (Coordinate, Coordinate)
boundingBox [] = error "boundingBox: can't find bounding box of empty list"
boundingBox cs =
    let minX = minimum (map (view xord) cs)
        maxX = maximum (map (view xord) cs)
        minY = minimum (map (view yord) cs)
        maxY = maximum (map (view yord) cs)
        minZ = minimum (map (view zord) cs)
        maxZ = maximum (map (view zord) cs)
    in ( review coordinate (minX, minY, minZ)
       , review coordinate (maxX, maxY, maxZ) )
