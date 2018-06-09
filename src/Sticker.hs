{-# LANGUAGE OverloadedStrings #-}
module Sticker(Sticker(Sticker), Sticker.id, pic, brand, country, founder, note, tags, stickersContent) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Database.SQLite.Simple as SQL

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

stickersContent :: Html
stickersContent = H.div ! class_ "tile is-ancestor" $ do
                    H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                    H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                    H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                    H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                    H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                    H.div ! class_ "tile is-ancestor" $ do
                        H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                        H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                        H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                        H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                        H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                    H.div ! class_ "tile is-ancestor" $ do
                        H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                        H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                        H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                        H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"
                        H.div ! class_ "tile is-parent" $ article ! class_ "tile is-child box" $ figure ! class_ "image is-128x128" $ img ! src "https://bulma.io/images/placeholders/256x256.png"