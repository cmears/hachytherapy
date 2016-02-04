module Hachytherapy.ReadBex (
    readBexFile
) where

import Control.Monad.State
import Data.List.Split
import qualified Data.Map as M
import Safe

import Hachytherapy.Coordinate
import Hachytherapy.Types

assert :: Monad m => String -> Bool -> m ()
assert _   True = return ()
assert msg False = error $ "assertion failed: " ++ msg

nearlyEq :: (Fractional a, Ord a) => a -> a -> Bool
nearlyEq x y = abs (x-y) < 0.00001

roughlyEq :: (Fractional a, Ord a) => a -> a -> Bool
roughlyEq x y = abs (x-y) < 0.2

assertEqual :: (Monad m, Show a, Show a1) => [Char] -> (a -> a1 -> Bool) -> a -> a1 -> m ()
assertEqual msg eq x y =
  case eq x y of
    True -> return ()
    False -> error $ "assertEqual failed (" ++ msg ++ "): " ++ show x ++ " != " ++ show y

readBexFile :: FilePath -> IO (DoseMap, (Ordinate, Ordinate))
readBexFile filepath = do
  blob <- lines <$> readFile filepath
  (allVals, gridSpacing) <- flip evalStateT blob $ do
    numSlices <- getInt "<Number of dose-grid slices>"
    upperLeft <- getDouble2 "<Upper-left X,Y of dose grid (mm)>"
    lowerRight <- getDouble2 "<Lower-right X,Y of dose grid (mm)>"
    gridSpacing <- getDouble "<Grid spacing along X and Y axes (mm)>"
    (nx,ny) <- getInt2 "<Number of grid points in X and Y directions>"

    let xCoords = take nx [ fst upperLeft, fst upperLeft + gridSpacing .. ]
    let yCoords = take ny [ snd upperLeft, snd upperLeft + gridSpacing .. ]
    assertEqual "checking xCoords" roughlyEq (last xCoords) (fst lowerRight)
    assertEqual "checking yCoords" roughlyEq (last yCoords) (snd lowerRight)

    let sliceCoords = [ (x,y) | x <- xCoords, y <- yCoords ]

    sliceVals <- forM [1..numSlices] $ \_ -> do
      z <- getDouble "<Z-coordinate of slice (mm)>"
      skipLine
      -- The (/100) is to convert from centiGray to Gray.
      vals <- map (/100) . map (readNote "reading slices") . words <$> getCurrentLine
      let coords = [ makeCoordinate (x,y,z) | (x,y) <- sliceCoords ]
      assertEqual "number of coords in slice" (==) (length vals) (length coords)
      return $ zip coords (vals :: [Double])
    return (concat sliceVals, gridSpacing)
  return (M.fromList allVals, (makeOrdinate gridSpacing, makeOrdinate gridSpacing))

type Blob a = StateT [String] IO a

getInt :: String -> Blob Int
getInt target = do
  rest <- getTarget target
  return $ readNote "getInt" rest

getTarget :: String -> Blob String
getTarget target = do
  ls <- get
  let n = length target
  assert ("couldn't find target " ++ show target) $ not (null ls)
  let (a,b) = splitAt n (head ls)
  skipLine
  if a == target
    then return b
    else getTarget target

getDouble :: String -> Blob Double
getDouble target = readNote "getDouble" <$> getTarget target

getInt2 :: String -> Blob (Int,Int)
getInt2 = get2

getDouble2 :: String -> Blob (Double,Double)
getDouble2 = get2

get2 :: Read a => String -> Blob (a,a)
get2 target = do
  rest <- getTarget target
  let [a,b] = splitOn ", " rest
  case (readMay a, readMay b) of
    (Just a', Just b') -> return (a', b')
    _ -> error $ "couldn't read (" ++ target ++ "): " ++ show [a,b]

skipLine :: Blob ()
skipLine = modify tail

getCurrentLine :: Blob String
getCurrentLine = head <$> get
