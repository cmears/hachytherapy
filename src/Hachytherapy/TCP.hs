module Hachytherapy.TCP
  ( logtcp
  , computeLogTCPMap
  , computeLogTCPVector
  , tcpDVH
  , tcpDVHSampledAlpha
  ) where

import qualified Data.Map as M

import Hachytherapy.Alpha
import Hachytherapy.Types
import Hachytherapy.Coordinate

import qualified Data.Vector.Unboxed as U




-- The half-life of I-125, in days.
halfLifeOfIodine125 :: Double
halfLifeOfIodine125 = 59.4

-- This is derived from the area under the curve of
-- the strength of the radiation.
lambda :: Double
lambda = ln2 / halfLifeOfIodine125

ln2 :: Double
ln2 = log 2

-- tumour doubling time, in days
-- value from sample code
tPot :: Double
tPot = 42

mu :: Double
mu = ln2/0.27*24

alphaOverBeta :: Double
alphaOverBeta = 3.1


-- Calculate the log of the TCP for a single "blob".  A "blob" might
-- be a voxel, or a quadrant, or a DVH bin.
--
-- In a call "logtcp dose alpha rho vol":
--
--   * dose is the dose received by the blob, in Gray;
--   * alpha is a measure of hypoxia, typically in the range
--       0.05 - 0.40;
--   * rho is the tumour cell density;
--   * vol is the volume of the blob, in mm³.

-- This function misbehaves at low doses, so just give a "negative
-- infinity" value instead.
logtcp :: Double -> Double -> Double -> Double -> Double
logtcp _dose _alpha _rho  vol | vol == 0.0 = 0.0
logtcp  dose _alpha _rho _vol | dose < 30 = -100
logtcp  dose  alpha  rho  vol = -thisNs
  where
    r0 = lambda * dose
    tCrit = -log (ln2 / (alpha * r0 * tPot)) / lambda
    exp1 = 1 - exp (-lambda * tCrit)
    exp2 = 1 - exp (-2 * lambda * tCrit)
    exp3 = 1 - exp (- (mu+lambda) * tCrit)
    d = r0 / lambda * exp1
    re = 1 + 2 * r0 * lambda / (alphaOverBeta * (mu - lambda) * exp1)
           * (exp2 / (2 * lambda) - exp3 / (lambda + mu));
    -- Calculate the number of surviving clonogens for this dose bin
    thisNs = rho * vol * exp (-alpha * d * re + ln2 * tCrit / tPot)

computeLogTCPMap :: Double -> (Coordinate -> Double) -> Double -> DoseMap -> LogTCPMap
computeLogTCPMap alpha rhofunc pointVolume dm =
    M.mapWithKey (\c d -> logtcp d alpha (rhofunc c) pointVolume) dm

computeLogTCPVector :: Double -> U.Vector Double -> Double -> U.Vector Dose -> U.Vector Double
computeLogTCPVector alpha rhoVec pointVolume dv =
    U.imap (\i d -> logtcp d alpha (rhoVec U.! i) pointVolume) dv

-- Calculate the TCP for a dose volume histogram.
-- It's assumed that the dose given for each bucket -- the first value
-- in the pair -- is the dose you want attributed to that bucket.
-- That is, if you want to shift it by 0.5, for example, you've
-- already done it.
tcpDVH :: [(Double,Double)] -> Double -> Double -> Double
tcpDVH dvh a rho = exp $ sum $ map (\(dose,vol) -> logtcp dose a rho vol) dvh

-- Calculate the TCP for a dose volume histogram, using alpha-sampling.
tcpDVHSampledAlpha :: [(Double, Double)] -> Double -> Double
tcpDVHSampledAlpha dvh rho =
    let totalweight = sum (map alphaWeight alphaValues)
    in sum (map (\ak -> alphaWeight ak * tcpDVH dvh ak rho) alphaValues)
         / totalweight
