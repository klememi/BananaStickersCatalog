{-# LANGUAGE OverloadedStrings #-}
module CatalogSpec where

import Test.Hspec
import Test.Hspec.Wai            as W
import Database.SQLite.Simple    as SQL
import Catalog
import Network.Wai (Application)
import qualified Web.Scotty      as S

-- spec :: Spec
-- spec = with app $ do
--     describe "GET /" $ do
--         it "responds with 200" $ do
--             get "/" `shouldRespondWith` 200
