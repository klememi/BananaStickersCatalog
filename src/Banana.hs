{-# LANGUAGE OverloadedStrings #-}
module Banana(catalog) where

import Prelude as P
import Country
import Sticker as ST
import Brand
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Monoid (mempty)
import Control.Monad.IO.Class (liftIO)
import Data.List(group, sort, groupBy)
import Database.SQLite.Simple as SQL
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty as S
import Network.Wai.Middleware.Static
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Network.Wai.Parse

data Page = Index
          | Countries
          | Brands
          | Country
          | Brand

catalog :: IO()
catalog = do
    conn <- SQL.open "stickers.db"
    scotty 3000 $ do
        middleware $ staticPolicy (noDots >-> addBase "static")
        get "/" $ do
            stickers <- liftIO $ query_ conn "select * from stickers"
            renderPage Index stickers
        get "/brands" $ do
            stickers <- liftIO $ query_ conn "select * from stickers"
            renderPage Brands stickers
        get "/countries" $ do
            stickers <- liftIO $ query_ conn "select * from stickers"
            renderPage Countries stickers
        post "/addSticker" $ do
            brand   <- S.param "brand"
            country <- S.param "country"
            founder <- S.param "founder"
            note    <- S.param "note"
            fs      <- files
            let pics       = [fileContent fi | (_, fi) <- fs]
            let pic        = BL.toStrict $ pics !! 0
            let encodedPic = C8.unpack $ B64.encode pic
            let tags       = "tag"
            liftIO $ execute conn "insert into stickers(pic, brand, country, founder, note, tags) values (?, ?, ?, ?, ?, ?)" (encodedPic :: String, brand :: String, country :: String, founder :: String, note :: String, tags :: String)
            S.redirect "/"
        get "/country/:country" $ do
            country <- S.param "country"
            stickers <- liftIO $ query conn "select * from stickers where country = ?" [country :: String]
            renderPage Index stickers
        get "/brand/:brand" $ do
            brand <- S.param "brand"
            stickers <- liftIO $ query conn "select * from stickers where brand = ?" [brand :: String]
            renderPage Index stickers
        get "/search" $ do
            keyword <- S.param "keyword"
            stickers <- liftIO $ queryNamed conn "select * from stickers where brand = :k or country = :k or founder = :k or note = :k" [":k" := (keyword :: String)]
            renderPage Index stickers
    close conn

renderPage :: Page -> [Sticker] -> ActionM()
renderPage Index stickers             = getPage (stickersContent stickers) stickers
renderPage Countries stickers         = getPage (countriesHtml $ unique country stickers) stickers
renderPage Brands stickers            = getPage (brandsHtml $ unique brand stickers) stickers

unique :: (Sticker -> String) -> [Sticker] -> [[String]]
unique element stickers = groupBy (\x y -> x !! 0 == y !! 0) $ P.map (\x -> x !! 0) $ group $ sort $ P.map element stickers

getPage :: Html -> [Sticker] -> ActionM()
getPage getContent stickers = S.html . renderHtml $ do
        docTypeHtml ! lang "en" $ do
            --  HEAD
            H.head $ do
                meta ! charset "utf-8"
                H.title "Banana Stickers Catalog"
                meta ! A.name "description" ! content "Banana Stickers Catalog"
                meta ! A.name "author" ! content "Michal Klement"
                link ! rel "stylesheet" ! href "../css/bulma.css"
                link ! rel "stylesheet" ! href "../css/flag-icon.css"
                script ! defer "" ! src "https://use.fontawesome.com/releases/v5.0.13/js/all.js" $ mempty
            --  BODY
            H.body $ do
                section ! class_ "hero is-warning is-bold" $ do
                    H.div ! class_ "hero-head" $ H.header ! class_ "navbar" $ H.div ! class_ "container" $ do
                        H.div ! class_ "navbar-brand" $ do
                            a ! class_ "navbar-item" ! href "/" $ do
                                H.span ! class_ "icon is-medium" $ i ! class_ "far fa-sticky-note" $ mempty
                                "Banana stickers"
                            H.span ! class_ "navbar-burger burger" ! dataAttribute "target" "navbarMenuHeroC" $ do
                                H.span mempty
                                H.span mempty
                                H.span mempty
                        H.div ! A.id "navbarMenuHeroC" ! class_ "navbar-menu" $ H.div ! class_ "navbar-end" $ do
                            H.span ! class_ "navbar-item" $ H.div ! class_ "field" $ H.form ! action "/search" ! method "get" $ p ! class_ "control has-icons-left" $ do
                                input ! class_ "input" ! type_ "text" ! A.name "keyword" ! placeholder "Search"
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
                            H.div ! class_ "level-item has-text-centered" $ H.div $ a ! href "/" $ do
                                p ! class_ "heading" $ "Stickers"
                                p ! class_ "title" $ toHtml $ show $ length stickers
                            H.div ! class_ "level-item has-text-centered" $ H.div $ a ! href "/brands" $ do
                                p ! class_ "heading" $ "Brands"
                                p ! class_ "title" $ toHtml $ show $ sum $ P.map (\x -> length x) $ unique brand stickers
                            H.div ! class_ "level-item has-text-centered" $ H.div $ a ! href "/countries" $ do
                                p ! class_ "heading" $ "Countries of origin"
                                p ! class_ "title" $ toHtml $ show $ sum $ P.map (\x -> length x) $ unique country stickers
                section ! class_ "section" $ H.div ! class_ "container is-desktop" $ getContent
                H.div ! class_ "modal" ! A.id "addModal" $ do
                    H.div ! class_ "modal-background" $ mempty
                    H.div ! class_ "modal-card" $ do
                        H.header ! class_ "modal-card-head" $ do
                            p ! class_ "modal-card-title" $ "Add new sticker"
                            button ! class_ "delete" ! onclick "(function() {document.getElementById(\"addModal\").classList.remove(\"is-active\");})();" $ mempty
                        section ! class_ "modal-card-body" $ H.div ! class_ "container is-fluid" $ H.form ! action "/addSticker" ! method "post" ! enctype "multipart/form-data" ! A.id "addForm" $ do
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Picture"
                                H.div ! class_ "file is-small has-name is-fullwidth" $ H.label ! class_ "file-label" $ do
                                    input ! class_ "file-input" ! type_ "file" ! A.id "file" ! A.name "file" ! accept ".jpg, .jpeg, .png" ! onchange "(function() {document.getElementById(\"filename\").innerHTML = document.getElementById(\"file\").files[0].name;})();" ! required ""
                                    H.span ! class_ "file-cta" $ do
                                        H.span ! class_ "file-icon" $ i ! class_ "fas fa-upload" $ mempty
                                        H.span ! class_ "file-label" $ "Browse files"
                                    H.span ! class_ "file-name" ! A.id "filename" $ mempty
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Brand"
                                H.div ! class_ "control" $ input ! class_ "input is-small" ! type_ "text" ! placeholder "what brand it is?" ! A.name "brand" ! required ""
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Country"
                                H.div ! class_ "control" $ H.div ! class_ "select is-small is-fullwidth" $ select ! A.name "country" $ countriesList
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Founder"
                                H.div ! class_ "control" $ input ! class_ "input is-small" ! type_ "text" ! placeholder "who found it?" ! A.name "founder" ! required ""
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Note"
                                H.div ! class_ "control" $ textarea ! class_ "textarea is-small" ! A.name "note" ! type_ "text" ! placeholder "some special note or story which led to this sticker" ! required "" ! rows "4" $ mempty
                        footer ! class_ "modal-card-foot" $ do
                            button ! class_ "button is-success" ! type_ "submit" ! A.form "addForm" $ "Add"
                            button ! class_ "button" ! onclick "(function() {document.getElementById(\"addModal\").classList.remove(\"is-active\");})();" $ "Cancel"
            --  FOOTER
            footer ! class_ "footer" $ H.div ! class_ "container" $ H.div ! class_ "content has-text-centered" $ p $ a ! href "https://bulma.io" $ img ! src "https://bulma.io/images/made-with-bulma--semiblack.png" ! alt "Made with Bulma" ! width "128" ! height "24"

