{-|
Module      : HtmlPage
Description : Module with functions for rendering HTML pages
Copyright   : (c) Michal Klement, 2018
License     : MIT
Maintainer  : klememi1@fit.cvut.cz

This module includes data constructor for different web pages as well as functions for rendering them.
-}
{-# LANGUAGE OverloadedStrings #-}
module HtmlPage(Page(Index, Countries, Brands, Type), renderPage, randomSample, unique) where

import Prelude                                              as P
import Country
import Sticker
import Brand
import Web.Scotty                                           as S
import Text.Blaze.Html5                                     as H
import Text.Blaze.Html5.Attributes                          as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.List                     (group, sort, groupBy)
import System.Random                 (randomRIO)
import Data.Monoid                   (mempty)
import System.IO.Unsafe

-- |Data type representing HTML page
data Page = Index     -- ^ Page displaying stickers
          | Countries -- ^ Page displaying countries list
          | Brands    -- ^ Page displaying brands list
          | Type      -- ^ Page displaying stickers of a single brand or country

-- |Renders a web page
renderPage :: Page      -- ^ Page to be rendered
           -> [Sticker] -- ^ List of stickers to be processed
           -> ActionM() -- ^ The return value
renderPage Index stickers     = getPage (stickersContent $ unsafePerformIO $ randomSample 10 stickers) stickers
renderPage Countries stickers = getPage (countriesHtml $ unique country stickers) stickers
renderPage Brands stickers    = getPage (brandsHtml $ unique brand stickers) stickers
renderPage Type stickers      = getPage (stickersContent stickers) stickers

-- |Prepares a page for rendering
getPage :: Html      -- ^ HTML content to be displayed in the body of the page
        -> [Sticker] -- ^ List of stickers to be processed
        -> ActionM() -- ^ The return value
getPage bodyContainerContent stickers = S.html . renderHtml $ do
                                            headerContent
                                            bodyContent bodyContainerContent stickers
                                            footerContent

-- |Makes a list of unique elements from a list grouped by the first letter
unique :: (Sticker -> String) -- ^ Element of the sticker to be returned
       -> [Sticker]           -- ^ List of stickers to be processed
       -> [[String]]          -- ^ The return value
unique element stickers = groupBy (\x y -> x !! 0 == y !! 0) $ P.map (\x -> x !! 0) $ group $ sort $ P.map element stickers

-- |Makes a random sample from given list
randomSample :: Int    -- ^ Size of the random sample
             -> [a]    -- ^ List for the random sample
             -> IO [a] -- ^ The return value
randomSample 0 x  = pure []
randomSample _ [] = pure []
randomSample k x  = do let m = P.min k (length x)
                       i <- randomRIO (0, length x - 1)
                       let (a, e:b) = splitAt i x
                       l <- randomSample (m-1) (a ++ b)
                       pure (e : l)

-- |Header HTML content
headerContent :: Html
headerContent = docTypeHtml ! lang "en" $ do
                    H.head $ do
                        meta ! charset "utf-8"
                        H.title "Banana Stickers Catalog"
                        meta ! A.name "description" ! content "Banana Stickers Catalog"
                        meta ! A.name "author" ! content "Michal Klement"
                        link ! rel "stylesheet" ! href "../css/bulma.css"
                        link ! rel "stylesheet" ! href "../css/flag-icon.css"
                        script ! defer "" ! src "https://use.fontawesome.com/releases/v5.0.13/js/all.js" $ mempty

-- |Body HTML content
bodyContent :: Html      -- ^ HTML content to be displayed in the body of the page
            -> [Sticker] -- ^ List of stickers to be processed
            -> Html      -- ^ The return value
bodyContent bodyContainerContent stickers = H.body $ do
                                                heroSection stickers
                                                bodyContainer bodyContainerContent
                                                addStickerModal

-- |Bulma Hero HTML section
heroSection :: [Sticker] -- ^ List of stickers to be processed
            -> Html      -- ^ The return value
heroSection stickers = section ! class_ "hero is-warning is-bold" $ do
                            heroHeader
                            heroBody stickers

-- |Bulma Hero header HTML content
heroHeader :: Html
heroHeader = H.div ! class_ "hero-head" $ H.header ! class_ "navbar" $ H.div ! class_ "container" $ do
                        H.div ! class_ "navbar-brand" $ do
                            a ! class_ "navbar-item" ! href "/" $ do
                                H.span ! class_ "icon is-medium" $ i ! class_ "far fa-sticky-note" $ mempty
                                "Banana stickers"
                        H.div ! A.id "navbarMenuHeroC" ! class_ "navbar-menu" $ H.div ! class_ "navbar-end" $ do
                            H.span ! class_ "navbar-item" $ H.div ! class_ "field" $ H.form ! action "/search" ! method "get" $ p ! class_ "control has-icons-left" $ do
                                input ! class_ "input" ! type_ "text" ! A.name "keyword" ! placeholder "Search"
                                H.span ! class_ "icon is-small is-left" $ i ! class_ "fas fa-search" $ mempty
                            a ! class_ "navbar-item" ! onclick "(function() {document.getElementById(\"addModal\").classList.add(\"is-active\");})();" $ H.span ! class_ "icon is-medium" $ i ! class_ "fas fa-lg fa-plus-square" $ mempty
                            -- loginDropdown

-- |Bulma Hero body HTML content
heroBody :: [Sticker] -- ^ List of stickers to be processed
         -> Html      -- ^ The return value
heroBody stickers = H.div ! class_ "hero-body" $ H.div ! class_ "container" $ do
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

-- |Login dropdown HTML
loginDropdown :: Html
loginDropdown = a ! class_ "navbar-item" $ H.div ! class_ "dropdown is-hoverable is-right" $ do
                    H.div ! class_ "dropdown-trigger" $ H.span ! class_ "icon is-medium" $ i ! class_ "fas fa-lg fa-sign-in-alt" $ mempty
                    H.div ! class_ "dropdown-menu" ! A.id "dropdown-menu4" $ H.div ! class_ "dropdown-content" $ H.form ! action "/login" ! method"post" ! A.id "loginForm" $ H.div ! class_ "dropdown-item" $ do
                        H.div ! class_ "field" $ p ! class_ "control has-icons-left" $ do
                            input ! class_ "input is-small" ! type_ "text" ! name "name" ! placeholder "Name" ! required ""
                            H.span ! class_ "icon is-small is-left" $ i ! class_ "fas fa-user" $ mempty
                        H.div ! class_ "field" $ p ! class_ "control has-icons-left" $ do
                            input ! class_ "input is-small" ! type_ "password" ! name "password" ! placeholder "Password" ! required ""
                            H.span ! class_ "icon is-small is-left" $ i ! class_ "fas fa-lock" $ mempty
                        H.div ! class_ "field" $ p ! class_ "control" $ button ! class_ "button is-small is-warning is-fullwidth" ! type_ "submit"! A.form "loginForm" $ "Login"

-- |Body HTML container
bodyContainer :: Html -- ^ HTML content to be displayed in the body
              -> Html -- ^ The return value
bodyContainer bodyContainerContent = section ! class_ "section" $ H.div ! class_ "container is-desktop" $ bodyContainerContent

-- |HTML modal container for adding sitckers
addStickerModal :: Html
addStickerModal = H.div ! class_ "modal" ! A.id "addModal" $ do
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
                                H.label ! class_ "label" $ "Finder"
                                H.div ! class_ "control" $ input ! class_ "input is-small" ! type_ "text" ! placeholder "who found it?" ! A.name "finder" ! required ""
                            H.div ! class_ "field" $ do
                                H.label ! class_ "label" $ "Note"
                                H.div ! class_ "control" $ textarea ! class_ "textarea is-small" ! A.name "note" ! type_ "text" ! placeholder "some special note or story which led to this sticker" ! required "" ! rows "4" $ mempty
                        footer ! class_ "modal-card-foot" $ do
                            button ! class_ "button is-success" ! type_ "submit" ! A.form "addForm" $ "Add"
                            button ! class_ "button" ! onclick "(function() {document.getElementById(\"addModal\").classList.remove(\"is-active\");})();" $ "Cancel"

-- |Footer HTML content
footerContent :: Html
footerContent = footer ! class_ "footer" $ H.div ! class_ "container" $ H.div ! class_ "content has-text-centered" $ p $ a ! href "https://bulma.io" $ img ! src "https://bulma.io/images/made-with-bulma--semiblack.png" ! alt "Made with Bulma" ! width "128" ! height "24"
