{-# LANGUAGE OverloadedStrings #-}
module Banana(catalog) where

import Prelude
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Monoid (mempty)
import Control.Monad.IO.Class (liftIO)
import Data.List
import Helpers
import Database.SQLite.Simple as SQL
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty as S
import Network.Wai.Middleware.Static
import Data.Digest.Pure.SHA
-- import Data.Time.Clock
-- import Data.Time.Calendar
-- import Data.Time.LocalTime

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

data Stats = Stats { stickersCount :: Int
                   , uniqBrands :: [String]
                   , uniqCountries :: [String]
                   }

catalog :: IO()
catalog = do
    conn <- SQL.open "stickers.db"
    scotty 3000 $ do
        middleware $ staticPolicy (noDots >-> addBase "static")
        get "/" $ do
            stickers <- liftIO $ query_ conn "SELECT * from stickers"
            let stickersCount = length stickers
            let uniqBrands = getUniqBrands stickers
            let uniqCountries = getUniqCountries stickers
            let currentStats = Stats stickersCount uniqBrands uniqCountries
            renderPage Index currentStats
        get "/brands" $ do
            stickers <- liftIO $ query_ conn "SELECT * from stickers"
            let stickersCount = length stickers
            let uniqBrands = getUniqBrands stickers
            let uniqCountries = getUniqCountries stickers
            let currentStats = Stats stickersCount uniqBrands uniqCountries
            renderPage Brands currentStats
        get "/countries" $ do
            stickers <- liftIO $ query_ conn "SELECT * from stickers"
            let stickersCount = length stickers
            let uniqBrands = getUniqBrands stickers
            let uniqCountries = getUniqCountries stickers
            let currentStats = Stats stickersCount uniqBrands uniqCountries
            renderPage Countries currentStats
        post "/addSticker" $ do
            brand <- S.param "brand"
            country <- S.param "country"
            founder <- S.param "founder"
            note <- S.param "note"
            liftIO $ execute conn "INSERT INTO stickers(brand, country, founder, note) VALUES (?, ?, ?, ?)" (brand :: String, country :: String, founder :: String, note :: String)
            S.redirect "/"
    close conn
        -- get "/country/:country" $ do
        --     country <- S.param "country"
        --     renderPage (Country country) initial
        -- get "/brand/:brand" $ do
        --     brand <- S.param "brand"
        --     renderPage (Brand brand) initial

-- renderPage :: Page -> ActionM()
renderPage Index stats             = S.html . renderHtml $ getPage getStickers stats
renderPage Countries stats         = S.html . renderHtml $ getPage (getCountries $ uniqCountries stats) stats
renderPage Brands stats            = S.html . renderHtml $ getPage (getBrands $ uniqBrands stats) stats
-- renderPage (Country country) state = S.html . renderHtml $ countryPage country state
-- renderPage (Brand brand) state     = S.html . renderHtml $ brandPage brand state

getUniqCountries stickers = P.map (\x -> x !! 0) $ group $ sort $ P.map country stickers

getUniqBrands stickers = P.map (\x -> x !! 0) $ group $ sort $ P.map brand stickers

getCountriesOptions :: Html
getCountriesOptions = mconcat $ P.map (\x -> option $ toHtml x) countries

getCountries :: [String] -> Html
getCountries countries = table ! class_ "table is-striped is-narrow is-hoverable is-fullwidth" $ do
                            thead $ tr $ th "A"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "A" x) countries)
                            thead $ tr $ th "B"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "B" x) countries)
                            thead $ tr $ th "C"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "C" x) countries)
                            thead $ tr $ th "D"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "D" x) countries)
                            thead $ tr $ th "E"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "E" x) countries)
                            thead $ tr $ th "F"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "F" x) countries)
                            thead $ tr $ th "G"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "G" x) countries)
                            thead $ tr $ th "H"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "H" x) countries)
                            thead $ tr $ th "I"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "I" x) countries)
                            thead $ tr $ th "J"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "J" x) countries)
                            thead $ tr $ th "K"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "K" x) countries)
                            thead $ tr $ th "L"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "L" x) countries)
                            thead $ tr $ th "M"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "M" x) countries)
                            thead $ tr $ th "N"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "N" x) countries)
                            thead $ tr $ th "O"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "O" x) countries)
                            thead $ tr $ th "P"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "P" x) countries)
                            thead $ tr $ th "Q"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "Q" x) countries)
                            thead $ tr $ th "R"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "R" x) countries)
                            thead $ tr $ th "S"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "S" x) countries)
                            thead $ tr $ th "T"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "T" x) countries)
                            thead $ tr $ th "U"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "U" x) countries)
                            thead $ tr $ th "V"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "V" x) countries)
                            thead $ tr $ th "W"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "W" x) countries)
                            thead $ tr $ th "X"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "X" x) countries)
                            thead $ tr $ th "Y"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "Y" x) countries)
                            thead $ tr $ th "Z"
                            tbody $ do
                                mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/country/" ++ x) $ do H.span ! class_ "flag-icon flag-icon-fr" $ mempty $ toHtml x) (filter (\x -> isPrefixOf "Z" x) countries)
                
                -- tbody $ tr $ td $ a ! class_ "link" $ do
                --     H.span ! class_ "flag-icon flag-icon-fr" $ mempty
                --     " France"

getBrands :: [String] -> Html
getBrands brands = table ! class_ "table is-striped is-narrow is-fullwidth" $ do
                    thead $ tr $ th "A"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "A" x) brands)
                    thead $ tr $ th "B"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "B" x) brands)
                    thead $ tr $ th "C"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "C" x) brands)
                    thead $ tr $ th "D"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "D" x) brands)
                    thead $ tr $ th "E"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "E" x) brands)
                    thead $ tr $ th "F"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "F" x) brands)
                    thead $ tr $ th "G"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "G" x) brands)
                    thead $ tr $ th "H"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "H" x) brands)
                    thead $ tr $ th "I"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "I" x) brands)
                    thead $ tr $ th "J"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "J" x) brands)
                    thead $ tr $ th "K"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "K" x) brands)
                    thead $ tr $ th "L"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "L" x) brands)
                    thead $ tr $ th "M"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "M" x) brands)
                    thead $ tr $ th "N"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "N" x) brands)
                    thead $ tr $ th "O"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "O" x) brands)
                    thead $ tr $ th "P"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "P" x) brands)
                    thead $ tr $ th "Q"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "Q" x) brands)
                    thead $ tr $ th "R"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "R" x) brands)
                    thead $ tr $ th "S"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "S" x) brands)
                    thead $ tr $ th "T"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "T" x) brands)
                    thead $ tr $ th "U"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "U" x) brands)
                    thead $ tr $ th "V"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "V" x) brands)
                    thead $ tr $ th "W"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "W" x) brands)
                    thead $ tr $ th "X"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "X" x) brands)
                    thead $ tr $ th "Y"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "Y" x) brands)
                    thead $ tr $ th "Z"
                    tbody $ do
                        mconcat $ P.map (\x -> tr $ td $ a ! class_ "link" ! (href $ toValue $ "/brand/" ++ x) $ toHtml x) (filter (\x -> isPrefixOf "Z" x) brands)

-- getBrandsCount :: Html
getBrandsCount stickers = length $ group $ sort $ P.map brand stickers

-- getStickers :: Html
getStickers = H.div ! class_ "tile is-ancestor" $ do
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

-- getPage :: Html -> Html
getPage getContent stats = do
        docTypeHtml ! lang "en" $ do
            --  HEAD
            H.head $ do
                meta ! charset "utf-8"
                H.title "Banana Stickers Catalog"
                meta ! A.name "description" ! content "Banana Stickers Catalog"
                meta ! A.name "author" ! content "Michal Klement"
                link ! rel "stylesheet" ! href "css/bulma.css"
                link ! rel "stylesheet" ! href "css/flag-icon.css"
                script ! defer "" ! src "https://use.fontawesome.com/releases/v5.0.13/js/all.js" $ mempty
            --  BODY
            H.body $ do
                section ! class_ "hero is-warning is-bold" $ do
                    H.div ! class_ "hero-head" $ H.header ! class_ "navbar" $ H.div ! class_ "container" $ do
                        H.div ! class_ "navbar-brand" $ do
                            a ! class_ "navbar-item" ! href "/" $ "Logo will be here"
                            H.span ! class_ "navbar-burger burger" ! dataAttribute "target" "navbarMenuHeroC" $ do
                                H.span mempty
                                H.span mempty
                                H.span mempty
                        H.div ! A.id "navbarMenuHeroC" ! class_ "navbar-menu" $ H.div ! class_ "navbar-end" $ do
                            H.span ! class_ "navbar-item" $ H.div ! class_ "field" $ p ! class_ "control has-icons-left" $ do
                                input ! class_ "input" ! type_ "text" ! placeholder "Search"
                                H.span ! class_ "icon is-small is-left" $ i ! class_ "fas fa-search" $ mempty
                            a ! class_ "navbar-item" ! onclick "(function() {document.getElementById(\"addModal\").classList.add(\"is-active\");})();" $ H.span ! class_ "icon is-medium" $ i ! class_ "fas fa-lg fa-plus-square" $ mempty
                            a ! class_ "navbar-item" $ H.div ! class_ "dropdown is-hoverable is-right" $ do
                                H.div ! class_ "dropdown-trigger" $ H.span ! class_ "icon is-medium" $ i ! class_ "fas fa-lg fa-sign-in-alt" $ mempty
                                H.div ! class_ "dropdown-menu" ! A.id "dropdown-menu4" $ H.div ! class_ "dropdown-content" $ H.div ! class_ "dropdown-item" $ do
                                    H.div ! class_ "field" $ p ! class_ "control has-icons-left" $ do
                                        input ! class_ "input is-small" ! type_ "text" ! placeholder "Name"
                                        H.span ! class_ "icon is-small is-left" $ i ! class_ "fas fa-user" $ mempty
                                    H.div ! class_ "field" $ p ! class_ "control has-icons-left" $ do
                                        input ! class_ "input is-small" ! type_ "password" ! placeholder "Password"
                                        H.span ! class_ "icon is-small is-left" $ i ! class_ "fas fa-lock" $ mempty
                                    H.div ! class_ "field" $ p ! class_ "control" $ button ! class_ "button is-small is-warning is-fullwidth" $ "Login"
                    H.div ! class_ "hero-body" $ H.div ! class_ "container" $ do
                        h1 ! class_ "title" $ "Banana Stickers Catalog"
                        h2 ! class_ "subtitle" $ "MI-AFP semestral project by Michal Klement"
                        nav ! class_ "level" $ do
                            H.div ! class_ "level-item has-text-centered" $ H.div $ do
                                p ! class_ "heading" $ "Stickers"
                                p ! class_ "title" $ toHtml $ show $ stickersCount stats
                            H.div ! class_ "level-item has-text-centered" $ H.div $ a ! href "/brands" $ do
                                p ! class_ "heading" $ "Brands"
                                p ! class_ "title" $ toHtml $ show $ length $ uniqBrands stats
                            H.div ! class_ "level-item has-text-centered" $ H.div $ a ! href "/countries" $ do
                                p ! class_ "heading" $ "Countries of origin"
                                p ! class_ "title" $ toHtml $ show $ length $ uniqCountries stats
                            -- H.div ! class_ "level-item has-text-centered" $ H.div $ do
                            --     p ! class_ "heading" $ "Visits"
                            --     p ! class_ "title" $ "789"
                section ! class_ "section" $ H.div ! class_ "container is-desktop" $ do
                    getContent
                H.div ! class_ "modal" ! A.id "addModal" $ do
                    H.div ! class_ "modal-background" $ mempty
                    H.div ! class_ "modal-card" $ do
                        H.header ! class_ "modal-card-head" $ do
                            p ! class_ "modal-card-title" $ "Add new sticker"
                            button ! class_ "delete" ! onclick "(function() {document.getElementById(\"addModal\").classList.remove(\"is-active\");})();" $ mempty
                        section ! class_ "modal-card-body" $ H.div ! class_ "container is-fluid" $ H.form ! action "/addSticker" ! method "post" ! A.id "addForm" $ do
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Picture"
                                H.div ! class_ "file is-small has-name is-fullwidth" $ H.label ! class_ "file-label" $ do
                                    input ! class_ "file-input" ! type_ "file" ! A.name "pic"
                                    H.span ! class_ "file-cta" $ do
                                        H.span ! class_ "file-icon" $ i ! class_ "fas fa-upload" $ mempty
                                        H.span ! class_ "file-label" $ "Browse files"
                                    H.span ! class_ "file-name" $ "Screen Shot 2017-07-29 at 15.54.25.png"
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Brand"
                                H.div ! class_ "control" $ input ! class_ "input is-small" ! type_ "text" ! placeholder "what brand it is?" ! A.name "brand"
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Country"
                                H.div ! class_ "control" $ H.div ! class_ "select is-small is-fullwidth" $ select ! A.name "country" $ do
                                    getCountriesOptions
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Founder"
                                H.div ! class_ "control" $ input ! class_ "input is-small" ! type_ "text" ! placeholder "who found it?" ! A.name "founder"
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Note"
                                H.div ! class_ "control" $ textarea ! class_ "textarea is-small" ! A.name "note" ! type_ "text" ! placeholder "some special note or story which led to this sticker" ! rows "4" $ mempty
                        footer ! class_ "modal-card-foot" $ do
                            button ! class_ "button is-success" ! type_ "submit" ! A.form "addForm" $ "Add"
                            button ! class_ "button" ! onclick "(function() {document.getElementById(\"addModal\").classList.remove(\"is-active\");})();" $ "Cancel"
            --  FOOTER
            footer ! class_ "footer" $ H.div ! class_ "container" $ H.div ! class_ "content has-text-centered" $ p $ a ! href "https://bulma.io" $ img ! src "https://bulma.io/images/made-with-bulma--semiblack.png" ! alt "Made with Bulma" ! width "128" ! height "24"

