{-|
Module      : HtmlGen
Description : Module with helper functions generating HTML
Copyright   : (c) Michal Klement, 2018
License     : BSD3
Maintainer  : klememi1@fit.cvut.cz

This module includes helper functions which generate HTML.
-}
{-# LANGUAGE OverloadedStrings #-}
module HtmlGen(elementsHtml) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Prelude as P

-- |Generates HTML for all available elements
elementsHtml :: (String -> Html) -- ^
             -> [[String]] -- ^ List of lists of elements grouped by the first letter
             -> Html       -- ^ The return value
elementsHtml mapF list = table ! class_ "table is-striped is-narrow is-hoverable is-fullwidth" $ mconcat $ P.map (\x -> (elementsMap mapF) x) list

-- |Generates HTML for a list of elements
elementsMap :: (String -> Html) -- ^
            -> [String]         -- ^ List of elements
            -> Html             -- ^ The return value
elementsMap mapF list = do thead $ tr $ th $ toHtml $ firstLetter list
                           tbody $ mconcat $ P.map (\x -> mapF x) list

-- |Gives first letter of list of elements
firstLetter :: [String] -- ^ List of elements names to be processed
            -> Char     -- ^ The return value
firstLetter list = list !! 0 !! 0
