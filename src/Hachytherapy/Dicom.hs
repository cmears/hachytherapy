-- This module is for parsing the text representation of DICOM files.
-- The text format is produced by "dcmdump +L <dicom-file>".

module Hachytherapy.Dicom
  ( readContourData
  , readDicomDoseMap
  )
  where

import Control.Lens hiding ((#))
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import Numeric
import Text.Printf
import Text.Regex
import Hachytherapy.Types
import Hachytherapy.Coordinate
import Hachytherapy.Contour

type Dump = [String]

readDump :: FilePath -> IO Dump
readDump = fmap lines . readFile

-- Read a dose map from a DICOM dose file.  Returns the map itself,
-- and the x- and y-spacing between points.
readDicomDoseMap :: FilePath -> IO (DoseMap, (Ordinate, Ordinate))
readDicomDoseMap dosepath = do
  dump <- readDump dosepath
  let gridFrameOffsetVector = ensureOne $ getList (0x3004, 0x000c) dump
      doseGridScaling = ensureOne $ ensureOne $ getList (0x3004, 0x000e) dump
      (xOrigin,yOrigin,zOrigin) = makeTriple . ensureOne $ getList (0x0020,0x0032) dump
      (xSpacing,ySpacing) = makePair . ensureOne $ getList (0x0028,0x0030) dump
      rows = ensureOne $ getUS (0x0028,0x0010) dump
      cols = ensureOne $ getUS (0x0028,0x0011) dump
      xCoords = genericTake cols [ xOrigin , xOrigin + xSpacing .. ]
      yCoords = genericTake rows [ yOrigin , yOrigin + ySpacing .. ]
      zCoords = map (zOrigin +) gridFrameOffsetVector
      coordinates = [ makeCoordinate (x,y,z) | z <- zCoords, y <- yCoords, x <- xCoords ]
      dosePixelData = ensureOne $ getPixelData (0x7fe0,0x0010) dump
      doses = map ((*doseGridScaling).fromIntegral) dosePixelData
  return (M.fromList (zip coordinates doses), (makeOrdinate xSpacing, makeOrdinate ySpacing))


ensureOne :: [a] -> a
ensureOne [x] = x
ensureOne  [] = error "ensureOne failed: got none"
ensureOne   _ = error "ensureOne failed: got many"

getPixelData :: (Integer,Integer) -> Dump -> [[Integer]]
getPixelData idx dump = do
  match <- filter (matchIndex idx) dump
  let innards = takeWhile (not . isSpace) . drop 15 $ match
      chunks = chunksOf 2 $ splitOn "\\" innards
      strings = map (\ [a,b] -> b++a ) chunks
      nums = map readHex' strings
  return nums

readContourData :: FilePath -> IO ContourData
readContourData path = do
  Just els <- getElementsFromFile path
  return (getContours els)

getContours :: [Element] -> ContourData
getContours = do
  -- First let's find the ROI metadata.
  tissueTypes <-
    descend1 (0x3006,0x0020) $ do
      descendMany (0xfffe,0xe000) $ do
         s <- asUnknownString <$> get1 (0x3006,0x0026)
         return $ case s of
           "[Prostate]" -> Just Prostate
           "[prostate]" -> Just Prostate
           "[Prostate ct]" -> Just Prostate
           "[Prostate ultrasound]" -> Just Prostate
           "[Prostate ulrasound]" -> Just Prostate
           "[Prostate ultrsound]" -> Just Prostate
           "[Post-lat prostate]" -> Nothing -- what is this??
           "[Urethra]" -> Just Urethra
           "[urethra]" -> Just Urethra
           "[Urethra ct]" -> Just Urethra
           "[Urethra ultrasound]" -> Just Urethra
           "[u/s urethra]" -> Just Urethra
           "[PTV]" -> Just PTV
           "[ptv]" -> Just PTV
           "[rectum]" -> Just Rectum
           "[Rectum]" -> Just Rectum
           "[Rectum ct]" -> Just Rectum
           "[Right Upper]" -> Nothing
           "[Left Upper]" -> Nothing
           "[Right Lower]" -> Nothing
           "[Left Lower]" -> Nothing
           "[R Upper]" -> Nothing
           "[L Upper]" -> Nothing
           "[Rt Upper]" -> Nothing
           "[Lt Upper]" -> Nothing
           "[Rt Lower]" -> Nothing
           "[Lt Lower]" -> Nothing
           "[R Lower]" -> Nothing
           "[L Lower]" -> Nothing
           "[Seminal Vesicles]" -> Nothing
           "[seminal Vesicles ct]" -> Nothing
           "[seminal vesicles ct]" -> Nothing
           "[Penile bulb]" -> Nothing
           "[penile bulb]" -> Nothing
           "[Penile bulb ct]" -> Nothing
           "[Calcification]" -> Nothing
           "[Calcifiacation]" -> Nothing
           "[calcification]" -> Nothing
           "[calcification ct]" -> Nothing
           "[calcification ultrasound]" -> Nothing
           "[PETSU]" -> Nothing -- what is this??
           "[Hoonsu]" -> Nothing -- what is this??
           "[HOONSU]" -> Nothing -- what is this??
           "[hoonsu]" -> Nothing -- what is this??
           "[Bladder]" -> Nothing
           "[Bladder ct]" -> Nothing
           _ -> error $ "getContours: unknown tissue type: " ++ s
  colourPathPairs <-
    descend1 (0x3006, 0x0039) $ do
      descendMany (0xfffe,0xe000) $ do
        colour <- maybe [127,127,127] asIntSeq <$> getAtMost1 (0x3006,0x002a)
        contours <-
          descend1 (0x3006,0x0040) $ do
            descendMany (0xfffe,0xe000) $ do
              c <- asDoubleSeq <$> get1 (0x3006,0x0050)
              let ts = map makeTriple . chunksOf 3 $ c
                  z = makeOrdinate (head ts ^. _3)
                  points = map (\ (x,y,_z) -> (makeOrdinate x, makeOrdinate y)) ts
              return (z,points)
        return (map fromIntegral colour, contours)
  return $ contourDataFromList $
    concatMap (\(mt,(c,p)) -> case mt of
                                Nothing -> []
                                Just t -> [ContourROI t c p]) $
    zip tissueTypes colourPathPairs

-- Find a hexpair and return its element.  Succeeds only if there is a
-- single element with that hexpair.
get1 :: HexPair -> [Element] -> Element
get1 target els =
  case filter (\e -> eHexPair e == target) els of
    [e] -> e
    _ -> error $ "get1: " ++ show target ++ "\n" ++ show els

-- Find a hexpair and return its element.  Succeeds only if there is a
-- single element with that hexpair, or it's missing.  If it's
-- missing, return Nothing.
getAtMost1 :: HexPair -> [Element] -> Maybe Element
getAtMost1 target els =
  case filter (\e -> eHexPair e == target) els of
    [e] -> Just e
    [] -> Nothing
    _ -> error $ "get1: " ++ show target ++ "\n" ++ show els

-- Find a hexpair and descend into it, running the given action
-- "inside" that hexpair.  Succeeds only if there is a single element
-- with the target hexpair.
descend1 :: HexPair -> ([Element] -> a) -> [Element] -> a
descend1 target action els =
  case filter (\e -> eHexPair e == target) els of
    [e] -> action (innerElements (eValue e))
    _ -> error $ "descend1: " ++ show target ++ "\n" ++ show (map eHexPair els)

-- Find many occurrences of a hexpair and run the given action
-- separately inside each one.
descendMany :: HexPair -> ([Element] -> a) -> [Element] -> [a]
descendMany target action els =
  let es = filter (\e -> eHexPair e == target) els
  in map (\e -> action (innerElements (eValue e))) es

asIntSeq :: Element -> [Int]
asIntSeq e =
  case eValue e of
    EVIntSeq xs -> xs
    _ -> error $ "asIntSeq: " ++ show e

asDoubleSeq :: Element -> [Double]
asDoubleSeq e =
  case eValue e of
    EVDoubleSeq xs -> xs
    _ -> error $ "asDoubleSeq: " ++ show e

asUnknownString :: Element -> String
asUnknownString e =
  case eValue e of
    EVUnknown s -> s
    _ -> error $ "asUnknownString: " ++ show e

innerElements :: ElementValue -> [Element]
innerElements (EVSequence els) = els
innerElements ev = error $ "innerElements: " ++ show ev


-- getRTROIObservationSequence :: DICOM [(Integer, String)]
-- getRTROIObservationSequence = do
--   enterSQ (0x3006,0x0080)
--   pairs <- foreachItem $ do
--     referencedROINumber <- getIS (0x3006,0x0084)
--     roiObservationLabel <- getSH (0x3006,0x0085)
--     return (referencedROINumber, roiObservationLabel)
--   return pairs

data Element = Element { eHexPair :: HexPair
                       , _eKind :: ElementKind
                       , eValue :: ElementValue }
  deriving (Show)

type HexPair = (Integer,Integer)

type ElementKind = String

data ElementValue = EVSequence [Element]
                  | EVIntSeq [Int]
                  | EVDoubleSeq [Double]
                  | EVUnknown String
  deriving (Show)

type ParserState = [(HexPair, ElementKind, String)]

getElement :: ParserState -> Maybe (Element, ParserState)
getElement [] = Nothing
getElement ((hp,ek,inner):rest) =
  case ek of
    "??" -> justGrabTheInnerString
    "na" -> case hp of
              (0xfffe,0xe000) -> grabInnerElements
              _ -> justGrabTheInnerString
    "CS" -> justGrabTheInnerString
    "DA" -> justGrabTheInnerString
    -- DS appears to be a variable-length sequence of doubles separated by backslashes
    --   e.g. [1.234\4.567]
    -- Note that the permitted syntax for these numbers is not the same as legal Haskell float literals
    --   (use the readNumber function defined below)
    "DS" ->
      case inner of
        "(no value available)" -> Just (Element hp ek (EVIntSeq []), rest)
        _ -> case (head inner, last inner) of
               ('[',']') ->
                 let vals = splitOn "\\" (init (tail inner))
                     ev = EVDoubleSeq $ map readNumber vals
                 in Just (Element hp ek ev, rest)
               _ -> error "Can't parse DS field"
    -- IS appears to be a variable-length sequence of integers separated by backslashes
    --   e.g. [1]
    --        [1\2\3]
    --        (no value available)
    "IS" ->
      case inner of
        "(no value available)" -> Just (Element hp ek (EVIntSeq []), rest)
        _ -> case (head inner, last inner) of
               ('[',']') ->
                 let vals = splitOn "\\" (init (tail inner))
                     ev = EVIntSeq $ map (round . readNumber) vals
                 in Just (Element hp ek ev, rest)
               _ -> error "Can't parse IS field"
              
    "LO" -> justGrabTheInnerString
    "LT" -> justGrabTheInnerString
    "OB" -> justGrabTheInnerString
    "PN" -> justGrabTheInnerString
    "SH" -> justGrabTheInnerString
    "SQ" -> grabInnerElements
    "TM" -> justGrabTheInnerString
    "UI" -> justGrabTheInnerString
    "UL" -> justGrabTheInnerString
    _ -> error $ "unknown element type: " ++ ek
  where
    justGrabTheInnerString = Just (Element hp ek (EVUnknown inner), rest)
    grabInnerElements = 
      let (inners, rest') = getElements rest
          e = Element hp ek (EVSequence inners)
      in Just (e, rest')
getElements :: ParserState -> ([Element], ParserState)
getElements [] = ([], [])
getElements ps =
  case getElement ps of
    Nothing -> ([], ps)
    Just (e, ps') ->
      case eHexPair e of
        -- End of inner sequence: stop, don't include.
        (0xfffe,0xe0dd) -> ([],ps')
        (0xfffe,0xe00d) -> ([],ps')
        _ -> let (es, ps'') = getElements ps'
             in (e:es, ps'')

hexDigit, hexQuad, hexPair :: String
hexDigit = "[0-9a-f]"
hexQuad = "(" ++ hexDigit ++ "{4}" ++ ")"
hexPair = "\\(" ++ hexQuad ++ "," ++ hexQuad ++ "\\)"

lineRegex :: Regex
lineRegex = mkRegex $ " *" ++ hexPair ++ " (..) ([^#]*)"

interpretLine :: String -> Maybe (HexPair, ElementKind, String)
interpretLine line = do
  [h1, h2, ek, val] <- matchRegex lineRegex line
  return ((readHex' h1, readHex' h2), ek, trimTrailingSpaces val)

interpretFile :: FilePath -> IO ParserState
interpretFile filename = do
  ls <- lines <$> readFile filename
  return $ catMaybes $ map interpretLine ls

getElementsFromFile :: FilePath -> IO (Maybe [Element])
getElementsFromFile filename = do
  (els, finalState) <- getElements <$> interpretFile filename
  case finalState of
    -- Everything was parsed.
    [] -> return (Just els)
    -- There was input left over: parse error.
    _ -> return Nothing
  

trimTrailingSpaces :: [Char] -> [Char]
trimTrailingSpaces = reverse . dropWhile isSpace . reverse


readHex' :: String -> Integer
readHex' = fst . head . readHex

makeTriple :: [a] -> (a,a,a)
makeTriple [a,b,c] = (a,b,c)
makeTriple (_:_:_:_) = error $ "makeTriple failed: too many elements"
makeTriple _         = error $ "makeTriple failed: too few elements"
makePair :: [a] -> (a,a)
makePair [a,b] = (a,b)
makePair (_:_:_) = error $ "makePair failed: too many elements"
makePair _       = error $ "makePair failed: too few elements"

getList :: (Integer,Integer) -> [String] -> [[Double]]
getList idx dump = do
    match <- filter (matchIndex idx) dump
    let innards = tail . takeWhile (/=']') . dropWhile (/='[') $ match
        nums = map readNumber $ splitOn "\\" innards
    return nums

getUS :: (Integer,Integer) -> [String] -> [Integer]
getUS idx dump = do
    match <- filter (matchIndex idx) dump
    let innards = takeWhile (not . isSpace) . drop 15 $ match
        num = read innards
    return num

readNumber :: String -> Double
readNumber ('-':'.':rest) = read $ "-0." ++ rest
readNumber ('.':rest) = read $ '0':'.':rest
readNumber s = read s

matchIndex :: (Integer,Integer) -> String -> Bool
matchIndex (a,b) line =
    take 11 (dropWhile isSpace line) == printf "(%04x,%04x)" a b
