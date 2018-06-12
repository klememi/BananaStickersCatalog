{-# LANGUAGE OverloadedStrings #-}
module CatalogSpec where

import           Test.Hspec
import           Test.Hspec.Wai            as W
import           Database.SQLite.Simple    as SQL
import           Catalog
import           Network.Wai (Application)
import qualified Web.Scotty                as S
import           Data.Text.Lazy

app :: IO Application
app = S.scottyApp $ do
        S.get "/" $ do
            S.text "index"

        S.get "/brands" $ do
            S.text "brands"

        S.get "/countries" $ do
            S.text "countries"

        S.post "/addSticker" $ do
            brand <- S.param "brand"
            country <- S.param "country"
            S.text $ pack (brand++" "++country)
            
        S.get "/country/:country" $ do
            country <- S.param "country"
            S.text country

        S.get "/brand/:brand" $ do
            brand <- S.param "brand"
            S.text brand

        S.get "/search" $ do
            keyword <- S.param "keyword"
            S.text keyword

spec :: Spec
spec = with app $ do
    describe "GET /" $ do
        it "responds with 200" $ do
            get "/" `shouldRespondWith` 200
        it "responds with index" $ do
            get "/" `shouldRespondWith` "index" {matchStatus = 200}
    describe "GET /brands" $ do
        it "responds with 200" $ do
            get "/brands" `shouldRespondWith` 200
        it "responds with brands" $ do
            get "/brands" `shouldRespondWith` "brands" {matchStatus = 200}
    describe "GET /countries" $ do
        it "responds with 200" $ do
            get "/countries" `shouldRespondWith` 200
        it "responds with countries" $ do
            get "/countries" `shouldRespondWith` "countries" {matchStatus = 200}
    describe "GET /country/:country" $ do
        it "responds with 200" $ do
            get "/country/Panama" `shouldRespondWith` 200
            get "/country/Colombia" `shouldRespondWith` 200
            get "/country/Peru" `shouldRespondWith` 200
        it "responds with country" $ do
            get "/country/Panama" `shouldRespondWith` "Panama" {matchStatus = 200}
            get "/country/Colombia" `shouldRespondWith` "Colombia" {matchStatus = 200}
            get "/country/Peru" `shouldRespondWith` "Peru" {matchStatus = 200}
    describe "GET /brand/:brand" $ do
        it "responds with 200" $ do
            get "/brand/Chiquita" `shouldRespondWith` 200
            get "/brand/DelMonte" `shouldRespondWith` 200
            get "/brand/Dole" `shouldRespondWith` 200
        it "responds with brand" $ do
            get "/brand/Chiquita" `shouldRespondWith` "Chiquita" {matchStatus = 200}
            get "/brand/DelMonte" `shouldRespondWith` "DelMonte" {matchStatus = 200}
            get "/brand/Dole" `shouldRespondWith` "Dole" {matchStatus = 200}
    describe "GET /search" $ do
        it "responds with 200" $ do
            get "/search?keyword=Test1" `shouldRespondWith` 200
            get "/search?keyword=Test2" `shouldRespondWith` 200
            get "/search?keyword=Test3" `shouldRespondWith` 200
        it "responds with keyword" $ do
            get "/search?keyword=Test1" `shouldRespondWith` "Test1" {matchStatus = 200}
            get "/search?keyword=Test2" `shouldRespondWith` "Test2" {matchStatus = 200}
            get "/search?keyword=Test3" `shouldRespondWith` "Test3" {matchStatus = 200}
    describe "POST /addSticker" $ do
        it "responds with 200" $ do
            postHtmlForm "/addSticker" [("brand", "Chiquita"), ("country", "Colombia"), ("finder", "Migel"), ("note", "note")] `shouldRespondWith` 200
        it "responds with parameters" $ do
            postHtmlForm "/addSticker" [("brand", "Chiquita"), ("country", "Colombia"), ("finder", "Migel"), ("note", "note")] `shouldRespondWith` "Chiquita Colombia" {matchStatus = 200}
            postHtmlForm "/addSticker" [("brand", "Dole"), ("country", "Peru"), ("finder", "Migel"), ("note", "note")] `shouldRespondWith` "Dole Peru" {matchStatus = 200}
    describe "wrong paths" $ do
        it "responds with 404" $ do
            get "/sticker" `shouldRespondWith` 404
            get "/pic" `shouldRespondWith` 404
            get "/id" `shouldRespondWith` 404
            get "/note" `shouldRespondWith` 404
