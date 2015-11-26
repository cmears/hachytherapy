{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Hachytherapy.Region 
              ( Region(..)
              , regionZone
              , regionQuadrant
              , quadrant
              , Zone(..)
              , Quadrant
              , unQuadrant
              , mkQuadrant
              , RegionData(..)
              , regionDataZoneMap
              , regionDataMap
              )
  where

import Control.Lens
import qualified Data.Map as M
import Data.Maybe
import Data.Serialize
import GHC.Generics

import Hachytherapy.Coordinate

data Region = Region { _regionZone :: Zone
                     , _regionQuadrant :: Quadrant }
  deriving (Show, Read, Generic, Eq, Ord)

data Zone = Apex | MidGland | Base
  deriving (Show, Eq, Ord, Read, Generic)

newtype Quadrant = Quadrant { unQuadrant :: (Int, Int) }
  deriving (Show, Read, Eq, Ord, Generic)

quadrant :: Prism' (Int,Int) Quadrant
quadrant = prism unQuadrant $ \(x,y) ->
             if 0 <= x && x <= 3 &&
                0 <= y && y <= 3
             then Right $ Quadrant (x,y)
             else Left (x,y)

regionZone :: Lens' Region Zone
regionZone = lens _regionZone $ \r z -> r { _regionZone = z }
regionQuadrant :: Lens' Region Quadrant
regionQuadrant = lens _regionQuadrant $ \r q -> r { _regionQuadrant = q }

mkQuadrant :: (Int, Int) -> Quadrant
mkQuadrant (x,y) = fromMaybe err $ (x,y) ^? quadrant
  where err = error $ "mkQuadrant: argument out of bounds: " ++ show (x,y)

data RegionData = RegionData { _regionDataMap :: M.Map Zone ((Ordinate,Ordinate),
                                                             (Ordinate,Ordinate))
                             , _regionDataZoneMap :: M.Map Ordinate Zone }
  deriving (Eq, Ord, Show, Read, Generic)

instance Serialize Quadrant
instance Serialize Zone
instance Serialize RegionData
instance Serialize Region

makeLenses ''RegionData
