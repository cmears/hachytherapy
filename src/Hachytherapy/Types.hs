{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Hachytherapy.Types where

import Control.Lens.TH
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics
import Data.Data
import Data.Serialize
import Diagrams.Prelude
--import Graphics.UI.Gtk hiding (Region)
--import FRP.Sodium
--import qualified Graphics.Rendering.Cairo as C

import Data.Vector
import qualified Data.Vector.Unboxed as U

import Hachytherapy.Coordinate
import Hachytherapy.Region

type DoseMap = M.Map Coordinate Dose

-- Dose in Gray
type Dose = Double

-- Map from coordinate to negative log of TCP for voxel.
type LogTCPMap = M.Map Coordinate Double

-- Volume in mm^3
type Volume = Double

type DVH = M.Map Dose Volume

type Contour = (Ordinate, [(Ordinate,Ordinate)])

-- A ContourROI is:
--   (tissueId, [r,g,b], contours)
data ContourROI = ContourROI {
    roiTissue :: Tissue
  , roiColour :: [Double]
  , roiContours :: [Contour]
  }
  deriving (Read, Show, Generic)

instance Serialize ContourROI

type ContourData = M.Map Tissue ContourROI

data Patient = Patient
  { _contourData :: ContourData
  , _tissueMap :: M.Map Coordinate Tissue
  , _variseedDoseMap :: DoseMap
  , _gridOffsets :: ([Ordinate], [Ordinate], [Ordinate])
  , _legalSeedCoordinates :: [Coordinate]
  , _tissueCoordinates :: M.Map Tissue (S.Set Coordinate)
  , _planeA1 :: (Ordinate, Ordinate)
  , _zOffsets :: [Ordinate]
  , _boundaries :: (Ordinate,Ordinate,Ordinate,Ordinate)
  , _doseBoundaries :: (Ordinate,Ordinate,Ordinate,Ordinate)
  , _sourceStrength :: SourceStrength
--  , _coordinateRegion :: M.Map Coordinate Region
  , _regionData :: RegionData
  , _biopsyData :: S.Set Region
  }
  deriving (Read, Show, Generic)

instance Serialize Patient

-- "PTV" means in the PTV but NOT prostate/urethra.
data Tissue = Prostate | Urethra | Rectum | PTV | Outside
  deriving (Eq,Ord,Show,Read,Generic,Enum)

data SeedState = SeedState {
      _doseMap :: DoseMap
    , _prostateDoseMap :: DoseMap
    , _urethraDoseMap :: DoseMap
    , _rectumDoseMap :: DoseMap
    , _ptvDoseMap :: DoseMap
    , _tcpMap :: M.Map Coordinate Double
    , _seedCoordinates :: [Coordinate]
    }
  deriving (Show, Eq)

data FastSeedState = FastSeedState {
      _doseVector :: U.Vector Dose
    , _coordVector :: Vector Coordinate
    , _rhoVector :: U.Vector Double
    , _prostateIndices :: U.Vector Int
    , _urethraIndices :: U.Vector Int
    , _rectumIndices :: U.Vector Int
    , _ptvIndices :: U.Vector Int
    , _tcpVector :: U.Vector Double
    , _seedList :: [Coordinate]
}
  deriving (Show)

newtype SourceStrength = SourceStrength Double
  deriving (Eq, Ord, Show, Read, Generic, Data, Typeable)

instance Serialize SourceStrength
instance Serialize Tissue

type Alpha = Double

makeLenses ''Patient
makeLenses ''SeedState
makeLenses ''FastSeedState

-- data Slice = Slice {
--       _sliceDrawingArea :: DrawingArea
--     , _sliceSurface :: C.Surface
--     , _slicePixmap :: IORef Pixmap
--     , _sliceId :: Int
-- }

-- makeLenses ''Slice

type Contours = M.Map Ordinate (M.Map Tissue (Path V2 Double))


  
tissueToChar :: Tissue -> String
tissueToChar Prostate = "P"
tissueToChar Urethra = "U"
tissueToChar Rectum = "R"
tissueToChar PTV = "T"
tissueToChar Outside = "O"
