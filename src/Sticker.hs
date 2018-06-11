{-|
Module      : Country
Description : Module with functionality for Sticker
Copyright   : (c) Michal Klement, 2018
License     : BSD3
Maintainer  : klememi1@fit.cvut.cz

This module includes constructor for data type Sticker and fuctions which generate HTML pages displaying stickers.
-}
{-# LANGUAGE OverloadedStrings #-}
module Sticker(Sticker(Sticker, id, pic, brand, country, finder, note, tags), stickersContent) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Database.SQLite.Simple as SQL
import Data.List.Split
import Prelude as P

-- |Data type representing sticker
data Sticker = Sticker { id      :: Int    -- ^ unqiue id
                       , pic     :: String -- ^ picture base64 encoded
                       , brand   :: String -- ^ brand
                       , country :: String -- ^ country of origin
                       , finder  :: String -- ^ finder
                       , note    :: String -- ^ note
                       , tags    :: String -- ^ tags semicolon separated
                       }

instance FromRow Sticker where
    fromRow = Sticker <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- |Generates HTML content for stickers
stickersContent :: [Sticker] -- ^ List of stickers to be processed
                -> Html      -- ^ The return value
stickersContent stickers = mconcat $ P.map (\x -> stickersGroup x) (chunksOf 5 stickers)

-- |Function for HTML content generation
stickersGroup :: [Sticker] -- ^ List of stickers to be processed
              -> Html      -- ^ The return value
stickersGroup group = H.div ! class_ "columns" $ mconcat $ P.map (\x -> stickerDiv x) group

-- |Generates HTML for a single sticker
stickerDiv :: Sticker -- ^ Sticker to be processed
           -> Html    -- ^ The return value
stickerDiv sticker = H.div ! class_ "column is-one-fifth" $ figure ! class_ "image is-4by5" $ do
                        img ! src (toValue $ "data:image/png;base64, " ++ (pic sticker))
                        H.div ! class_ "overlay" $ H.div ! class_ "overlay-text" $ do
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Brand"
                                toHtml $ brand sticker
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Country"
                                toHtml $ country sticker
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Finder"
                                toHtml $ finder sticker
