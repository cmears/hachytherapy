{-# LANGUAGE DeriveDataTypeable #-}

module Hachytherapy.Contour where

import Control.Monad.Catch
import Control.Lens
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Typeable
import Diagrams.Prelude as D
import Diagrams.TwoD.Path

import Hachytherapy.Coordinate
import Hachytherapy.Types

isProstate :: ContourData -> Coordinate -> Bool
isProstate cd c =     Prostate `elem` tissueType cd c &&
                  not (Urethra `elem` tissueType cd c)

isUrethra :: ContourData -> Coordinate -> Bool
isUrethra cd c = Urethra `elem` tissueType cd c

isPTV :: ContourData -> Coordinate -> Bool
isPTV cd c = PTV `elem` tissueType cd c &&
        not (Prostate `elem` tissueType cd c) &&
        not (Urethra `elem` tissueType cd c)

isRectum :: ContourData -> Coordinate -> Bool
isRectum cd c = Rectum `elem` tissueType cd c

-- Get the list of contours that this point is in.
tissueType :: ContourData -> Coordinate -> [Tissue]
tissueType cd coord = do
  roi <- M.elems cd
  (z',p) <- roiContours roi
  guard (coord ^. zord == z')
  let path = pointsToPath . map p2 . map (\(x,y) -> (ordinateToMillimetre x, ordinateToMillimetre y)) $ p
  guard (isInsideWinding (p2( ordinateToMillimetre (coord ^. xord)
                            , ordinateToMillimetre (coord ^. yord))) path)
  return $ roiTissue roi


getROI :: MonadThrow m => Tissue -> ContourData -> m ContourROI
getROI t cd =
  case M.lookup t cd of
    Just roi -> return roi
    Nothing -> throwM TissueMissing

addROI :: Tissue -> ContourROI -> ContourData -> ContourData
addROI t roi cd = M.insert t roi cd

contourDataFromList :: [ContourROI] -> ContourData
contourDataFromList rois =
  -- Make sure there is only one of each tissue type.
  let tissues = map roiTissue rois
  in if nub tissues == tissues
     then M.fromList [ (t, roi) | roi <- rois, let t = roiTissue roi ]
     else error "Multiple structures with same tissue type"

contourDataToList :: ContourData -> [ContourROI]
contourDataToList cd =
  M.elems cd

data TissueMissing = TissueMissing
  deriving (Show, Typeable)
instance Exception TissueMissing
