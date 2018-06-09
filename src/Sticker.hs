{-# LANGUAGE OverloadedStrings #-}
module Sticker(Sticker(Sticker), Sticker.id, pic, brand, country, founder, note, tags, stickersContent) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Database.SQLite.Simple as SQL
import Data.List.Split
import Prelude as P

data Sticker = Sticker { id :: Int
                       , pic :: String
                       , brand :: String
                       , country :: String
                       , founder :: String
                       , note :: String
                       , tags :: String
                       }

instance FromRow Sticker where
    fromRow = Sticker <$> field <*> field <*> field <*> field <*> field <*> field <*> field

stickersContent stickers = mconcat $ P.map (\x -> stickersGroup x) (chunksOf 5 stickers)

stickersGroup group = H.div ! class_ "tile is-ancestor" $ mconcat $ P.map (\x -> stickerDiv x) group

stickerDiv sticker = H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src (toValue $ "data:image/png;base64, " ++ (pic sticker))

-- stickersContent :: [Sticker] -> Html
-- stickersContent stickers = H.div ! class_ "tile is-ancestor" $ do
--                     H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                     H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                     H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                     H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                     H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                     H.div ! class_ "tile is-ancestor" $ do
--                         H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                         H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                         H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                         H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                         H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                     H.div ! class_ "tile is-ancestor" $ do
--                         H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                         H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                         H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                         H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
--                         H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"