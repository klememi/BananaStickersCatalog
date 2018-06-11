{-|
Module      : Catalog
Description : Main module of the application
Copyright   : (c) Michal Klement, 2018
License     : MIT
Maintainer  : klememi1@fit.cvut.cz

Main module with function for handling http requests.
-}
{-# LANGUAGE OverloadedStrings #-}
module Catalog(catalog) where

import           HtmlPage                        as HP
import           Sticker
import qualified Data.ByteString.Base64          as B64
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Char8           as C8
import           Control.Monad.IO.Class (liftIO)
import           Database.SQLite.Simple          as SQL
import           Web.Scotty                      as S
import           Network.Wai.Middleware.Static
import           Network.Wai.Parse
import           System.IO.Unsafe

-- |Main web application responding to requests
catalog :: IO ()
catalog = do conn <- SQL.open "stickers.db"
             scotty 3000 $ scottyCatalogApp conn
             close conn

-- |Scotty web app function
scottyCatalogApp :: Connection -- ^ SQLite database connection 
                 -> ScottyM () -- ^ The return value
scottyCatalogApp conn = do  middleware $ staticPolicy (noDots >-> addBase "static")
                            get "/" $ do
                                stickers   <- liftIO $ query_ conn "select * from stickers"
                                let randomS = unsafePerformIO $ randomSample 15 stickers
                                renderPage HP.Index randomS
                            get "/brands" $ do
                                stickers <- liftIO $ query_ conn "select * from stickers"
                                renderPage HP.Brands stickers
                            get "/countries" $ do
                                stickers <- liftIO $ query_ conn "select * from stickers"
                                renderPage HP.Countries stickers
                            post "/addSticker" $ do
                                brand   <- S.param "brand"
                                country <- S.param "country"
                                finder  <- S.param "finder"
                                note    <- S.param "note"
                                fs      <- files
                                let pics       = [fileContent fi | (_, fi) <- fs]
                                let pic        = BL.toStrict $ pics !! 0
                                let encodedPic = C8.unpack $ B64.encode pic
                                let tags       = "tag"
                                liftIO $ execute conn "insert into stickers(pic, brand, country, founder, note, tags) values (?, ?, ?, ?, ?, ?)" (encodedPic :: String, brand :: String, country :: String, finder :: String, note :: String, tags :: String)
                                S.redirect "/"
                            get "/country/:country" $ do
                                country  <- S.param "country"
                                stickers <- liftIO $ query conn "select * from stickers where country = ?" [country :: String]
                                renderPage HP.Index stickers
                            get "/brand/:brand" $ do
                                brand    <- S.param "brand"
                                stickers <- liftIO $ query conn "select * from stickers where brand = ?" [brand :: String]
                                renderPage HP.Index stickers
                            get "/search" $ do
                                keyword  <- S.param "keyword"
                                stickers <- liftIO $ queryNamed conn "select * from stickers where brand = :k or country = :k or founder = :k or note = :k" [":k" := (keyword :: String)]
                                renderPage HP.Index stickers