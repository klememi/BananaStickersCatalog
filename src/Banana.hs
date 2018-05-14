{-# LANGUAGE OverloadedStrings #-}
module Banana(catalog) where

import Prelude
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Monoid (mempty)
import Control.Monad.IO.Class (liftIO)
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

catalog :: IO()
catalog = scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/" $ do
        renderPage Index
    get "/brands" $ do
        renderPage Brands
    get "/countries" $ do
        renderPage Countries
    -- get "/country/:country" $ do
    --     country <- S.param "country"
    --     renderPage (Country country) initial
    -- get "/brand/:brand" $ do
    --     brand <- S.param "brand"
    --     renderPage (Brand brand) initial

-- renderPage :: Page -> IO()
renderPage Index             = S.html . renderHtml $ getPage getStickers
renderPage Countries         = S.html . renderHtml $ getPage getCountries
renderPage Brands            = S.html . renderHtml $ getPage getBrands
-- renderPage (Country country) state = S.html . renderHtml $ countryPage country state
-- renderPage (Brand brand) state     = S.html . renderHtml $ brandPage brand state

getCountriesOptions :: Html
getCountriesOptions = mconcat $ P.map (\x -> option $ toHtml x) countries

getCountries :: Html
getCountries = table ! class_ "table is-striped is-narrow is-hoverable is-fullwidth" $ do
                thead $ tr $ th "A"
                thead $ tr $ th "B"
                thead $ tr $ th "C"
                thead $ tr $ th "D"
                thead $ tr $ th "E"
                thead $ tr $ th "F"
                tbody $ tr $ td $ a ! class_ "link" $ do
                    H.span ! class_ "flag-icon flag-icon-fr" $ mempty
                    " France"
                thead $ tr $ th "G"
                thead $ tr $ th "H"
                thead $ tr $ th "I"
                thead $ tr $ th "J"
                thead $ tr $ th "K"
                thead $ tr $ th "L"
                thead $ tr $ th "M"
                thead $ tr $ th "N"
                thead $ tr $ th "O"
                thead $ tr $ th "P"
                thead $ tr $ th "Q"
                thead $ tr $ th "R"
                thead $ tr $ th "S"
                thead $ tr $ th "T"
                thead $ tr $ th "U"
                thead $ tr $ th "V"
                thead $ tr $ th "W"
                thead $ tr $ th "X"
                thead $ tr $ th "Y"
                thead $ tr $ th "Z"

getBrands :: Html
getBrands = table ! class_ "table is-striped is-narrow is-fullwidth" $ do
                thead $ tr $ th "A"
                tbody $ tr $ td $ a ! class_ "link" $ "Albert"
                thead $ tr $ th "B"
                thead $ tr $ th "C"
                tbody $ do
                    tr $ td $ a ! class_ "link" $ "Chiquita"
                    tr $ td $ a ! class_ "link" $ "Cobana"
                thead $ tr $ th "D"
                thead $ tr $ th "E"
                thead $ tr $ th "F"
                thead $ tr $ th "G"
                thead $ tr $ th "H"
                thead $ tr $ th "I"
                thead $ tr $ th "J"
                thead $ tr $ th "K"
                thead $ tr $ th "L"
                thead $ tr $ th "M"
                thead $ tr $ th "N"
                thead $ tr $ th "O"
                thead $ tr $ th "P"
                thead $ tr $ th "Q"
                thead $ tr $ th "R"
                thead $ tr $ th "S"
                thead $ tr $ th "T"
                thead $ tr $ th "U"
                thead $ tr $ th "V"
                thead $ tr $ th "W"
                thead $ tr $ th "X"
                thead $ tr $ th "Y"
                thead $ tr $ th "Z"

-- getStickers :: Html
getStickers =   H.div ! class_ "tile is-ancestor" $ do
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

getPage :: Html -> Html
getPage getContent = do
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
                                p ! class_ "title" $ "566"
                            H.div ! class_ "level-item has-text-centered" $ H.div $ a ! href "/brands" $ do
                                p ! class_ "heading" $ "Brands"
                                p ! class_ "title" $ "112"
                            H.div ! class_ "level-item has-text-centered" $ H.div $ a ! href "/countries" $ do
                                p ! class_ "heading" $ "Countries of origin"
                                p ! class_ "title" $ "39"
                            H.div ! class_ "level-item has-text-centered" $ H.div $ do
                                p ! class_ "heading" $ "Visits"
                                p ! class_ "title" $ "789"
                section ! class_ "section" $ H.div ! class_ "container is-desktop" $ do
                    getContent
                H.div ! class_ "modal" ! A.id "addModal" $ do
                    H.div ! class_ "modal-background" $ mempty
                    H.div ! class_ "modal-card" $ do
                        H.header ! class_ "modal-card-head" $ do
                            p ! class_ "modal-card-title" $ "Add new sticker"
                            button ! class_ "delete" ! onclick "(function() {document.getElementById(\"addModal\").classList.remove(\"is-active\");})();" $ mempty
                        section ! class_ "modal-card-body" $ H.div ! class_ "container is-fluid" $ do
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Picture"
                                H.div ! class_ "file is-small has-name is-fullwidth" $ H.label ! class_ "file-label" $ do
                                    input ! class_ "file-input" ! type_ "file" ! A.name "resume"
                                    H.span ! class_ "file-cta" $ do
                                        H.span ! class_ "file-icon" $ i ! class_ "fas fa-upload" $ mempty
                                        H.span ! class_ "file-label" $ "Browse files"
                                    H.span ! class_ "file-name" $ "Screen Shot 2017-07-29 at 15.54.25.png"
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Brand"
                                H.div ! class_ "control" $ input ! class_ "input is-small" ! type_ "text" ! placeholder "what brand it is?"
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Country"
                                H.div ! class_ "control" $ H.div ! class_ "select is-small is-fullwidth" $ select $ do
                                    getCountriesOptions
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Founder"
                                H.div ! class_ "control" $ input ! class_ "input is-small" ! type_ "text" ! placeholder "who found it?"
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Note"
                                H.div ! class_ "control" $ textarea ! class_ "textarea is-small" ! type_ "text" ! placeholder "some special note or story which led to this sticker" ! rows "4" $ mempty
                        footer ! class_ "modal-card-foot" $ do
                            button ! class_ "button is-success" $ "Add"
                            button ! class_ "button" ! onclick "(function() {document.getElementById(\"addModal\").classList.remove(\"is-active\");})();" $ "Cancel"
            --  FOOTER
            footer ! class_ "footer" $ H.div ! class_ "container" $ H.div ! class_ "content has-text-centered" $ p $ a ! href "https://bulma.io" $ img ! src "https://bulma.io/images/made-with-bulma--semiblack.png" ! alt "Made with Bulma" ! width "128" ! height "24"

