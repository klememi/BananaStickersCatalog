{-|
Module      : Brand
Description : Module with functionality for Brand
Copyright   : (c) Michal Klement, 2018
License     : BSD3
Maintainer  : klememi1@fit.cvut.cz

This module includes constructor for data type Brand and functions which generate HTML pages displaying list of brands.
-}
{-# LANGUAGE OverloadedStrings #-}
module Brand(Brand, brandsHtml) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Prelude as P
import HtmlGen

-- |Data type representing brand
newtype Brand = Brand String

-- |Generates HTML for all brands
brandsHtml :: [[String]] -- ^ List of lists of brands grouped by the first letter
           -> Html       -- ^
brandsHtml brands = (elementsHtml mapB) brands

-- |Generates HTML for a single brand
mapB :: String -- ^ Brand name
     -> Html   -- ^ The return value
mapB brand = tr $ td $ a ! class_ "link" ! href (toValue $ "/brand/" ++ brand) $ do
                   toHtml $ " " ++ brand
