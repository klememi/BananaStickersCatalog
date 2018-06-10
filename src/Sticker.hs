{-# LANGUAGE OverloadedStrings #-}
module Sticker(Sticker(Sticker), Sticker.id, pic, brand, country, finder, note, tags, stickersContent) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Database.SQLite.Simple as SQL
import Data.List.Split
import Prelude as P

data Sticker = Sticker { id :: Int
                       , pic :: String
                       , brand :: String
                       , country :: String
                       , finder :: String
                       , note :: String
                       , tags :: String
                       }

instance FromRow Sticker where
    fromRow = Sticker <$> field <*> field <*> field <*> field <*> field <*> field <*> field

stickersContent stickers = mconcat $ P.map (\x -> stickersGroup x) (chunksOf 5 stickers)

stickersGroup group = H.div ! class_ "columns" $ mconcat $ P.map (\x -> stickerDiv x) group

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