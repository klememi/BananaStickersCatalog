{-# LANGUAGE OverloadedStrings #-}
module Stats(Stats(Stats), stickersCount, uniqBrands, uniqCountries) where

data Stats = Stats { stickersCount :: Int
                   , uniqBrands :: [[String]]
                   , uniqCountries :: [[String]]
                   }
