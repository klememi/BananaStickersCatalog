module SupportSpec(spec) where

import Test.Hspec
import Data.List
import HtmlPage
import HtmlGen
import Sticker
import Country

sticker1 = Sticker { Sticker.id = 1,
                     pic        = "pic1",
                     brand      = "Beta",
                     country    = "Bolivia",
                     finder     = "Tom",
                     note       = "note",
                     tags       = "tag1"
                   }

sticker2 = Sticker { Sticker.id = 2,
                     pic        = "pic2",
                     brand      = "Alpha",
                     country    = "Brazilia",
                     finder     = "Tom",
                     note       = "note",
                     tags       = "tag1"
                   }

sticker3 = Sticker { Sticker.id = 3,
                     pic        = "pic3",
                     brand      = "Bravo",
                     country    = "Argentina",
                     finder     = "Tom",
                     note       = "note",
                     tags       = "tag1"
                   }

sticker4 = Sticker { Sticker.id = 4,
                     pic        = "pic4",
                     brand      = "Delta",
                     country    = "Chile",
                     finder     = "Tom",
                     note       = "note",
                     tags       = "tag1"
                   }

sticker5 = Sticker { Sticker.id = 5,
                     pic        = "pic5",
                     brand      = "Charlie",
                     country    = "Bulgaria",
                     finder     = "Tom",
                     note       = "note",
                     tags       = "tag1"
                   }

stickers = [ sticker1, sticker2, sticker3, sticker4, sticker5 ]

spec :: Spec
spec = do
    describe "HtmlPage" $ do
        it "returns correct list of elements grouped by first letter" $ do
            (unique country stickers) `shouldBe` [ ["Argentina"], ["Bolivia", "Brazilia", "Bulgaria"], ["Chile"] ]
            (unique brand stickers) `shouldBe` [ ["Alpha"], ["Beta", "Bravo"], ["Charlie"], ["Delta"] ]
    describe "HtmlGen" $ do
        it "returns correct first letter" $ do
            (firstLetter []) `shouldBe` 'X'
            (firstLetter ["Alpha", "Argentina", "Armenia"]) `shouldBe` 'A'
            (firstLetter ["Bravo", "Brazilia", "Ben", "Bulldog"]) `shouldBe` 'B'
    describe "Country" $ do
        it "returns correct country code" $ do
            (countryCode "Peru") `shouldBe` "pe"
            (countryCode "Mali") `shouldBe` "ml"
            (countryCode "Panama") `shouldBe` "pa"
