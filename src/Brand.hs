{-# LANGUAGE OverloadedStrings #-}
module Brand(Brand, brandsHtml) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Prelude as P

newtype Brand = Brand String

brandsHtml :: [[String]] -> Html
brandsHtml brands = table ! class_ "table is-striped is-narrow is-hoverable is-fullwidth" $ countriesContent brands

countriesContent :: [[String]] -> Html
countriesContent countries = mconcat $ P.map (\x -> countriesC x) countries

countriesC :: [String] -> Html
countriesC countries = do thead $ tr $ th $ toHtml $ firstLetter countries
                          tbody $ mapCountries countries

mapCountries :: [String] -> Html
mapCountries countries = mconcat $ P.map (\x -> mapC x) countries

mapC :: String -> Html
mapC country = tr $ td $ a ! class_ "link" ! href (toValue $ "/brand/" ++ country) $ do
                   toHtml $ " " ++ country

firstLetter :: [String] -> Char
firstLetter countries = countries !! 0 !! 0
