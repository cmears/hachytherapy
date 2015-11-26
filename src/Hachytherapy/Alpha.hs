module Hachytherapy.Alpha (alphaValues, alphaWeight) where

mu, alphabar, alphabar', sigma, sigma', tau :: Double
mu = 0.15
alphabar = mu
alphabar' = log alphabar
sigma = 0.04
sigma' = sqrt (log (1 + (sigma/alphabar)^two))
tau = mu * (sigma^two / (sigma^two + mu^two))

two :: Int
two = 2

alphaWeight :: Double -> Double
alphaWeight ak =
  recip ((ak - tau)*sigma'*sqrt(2*pi)) *
         exp ( (-(log (ak - tau) - alphabar')^two)
               / (2*sigma'^two) )

alphaValues :: [Double]
alphaValues = [0.01,0.02..0.4]

-- printAlphaTable :: IO ()
-- printAlphaTable =
--   forM_ alphaValues $ \a ->
--     printf "%.2f %.9f\n" a (alphaWeight a)

