{-# LANGUAGE OverloadedStrings #-}
module Country(Country, countriesList, countriesHtml) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Prelude as P

data Country = Country { name :: String
                       , code :: String
                       }

countries :: [Country]
countries = [ Country "Argentina" "ar",
              Country "Australia" "au",
              Country "Bahamas" "bs",
              Country "Barbados" "bb",
              Country "Belize" "bz",
              Country "Bermuda" "bm",
              Country "Bolivia" "bo",
              Country "Brazil" "br",
              Country "Cameroon" "cm",
              Country "Colombia" "co",
              Country "Costa Rica" "cr",
              Country "Côte d’Ivoire" "ci",
              Country "Cuba" "cu",
              Country "Dominican Republic" "do",
              Country "Ecuador" "ec",
              Country "El Salvador" "sv",
              Country "Equatorial Guinea" "gq",
              Country "Eritrea" "er",
              Country "Ethiopia" "et",
              Country "Fiji" "fj",
              Country "Gabon" "gn",
              Country "Gambia" "gm",
              Country "Ghana" "gh",
              Country "Guadeloupe" "gp",
              Country "Guatemala" "gt",
              Country "Guyana" "gy",
              Country "Haiti" "ht",
              Country "Honduras" "hn",
              Country "Indonesia" "id",
              Country "Jamaica" "jm",
              Country "Madagascar" "mg",
              Country "Mali" "ml",
              Country "Martinique" "mq",
              Country "Mauritania" "mr",
              Country "Mauritius" "mu",
              Country "Mexico" "mx",
              Country "Morocco" "ma",
              Country "New Zealand" "nz",
              Country "Nicaragua" "ni",
              Country "Nigeria" "ng",
              Country "Panama" "pa",
              Country "Papua New Guinea" "pg",
              Country "Paraguay" "py",
              Country "Peru" "pe",
              Country "Philippines" "ph",
              Country "Puerto Rico" "pr",
              Country "Senegal" "sn",
              Country "Seychelles" "sc",
              Country "Singapore" "sg",
              Country "South Africa" "za",
              Country "Sri Lanka" "lk",
              Country "Thailand" "th",
              Country "Trinidad & Tobago" "tt",
              Country "Uruguay" "uy",
              Country "Venezuela" "ve"
            ]

countriesList :: Html
countriesList = mconcat $ P.map (\x -> option $ toHtml $ Country.name x) countries

countriesHtml :: [[String]] -> Html
countriesHtml countries = table ! class_ "table is-striped is-narrow is-hoverable is-fullwidth" $ countriesContent countries

countriesContent countries = mconcat $ P.map (\x -> countriesC x) countries

countriesC countries = do thead $ tr $ th $ toHtml $ firstLetter countries
                          tbody $ mapCountries countries

mapCountries countries = mconcat $ P.map (\x -> mapC x) countries

mapC country = tr $ td $ a ! class_ "link" ! href (toValue $ "/country/" ++ country) $ do
                   H.span ! class_ (toValue $ "flag-icon flag-icon-" ++ (countryCode country)) $ mempty
                   toHtml $ " " ++ country

countryCode country = code where
                        code = Country.code $ c !! 0 where
                          c = filter (\x -> Country.name x == country) countries

firstLetter countries = countries !! 0 !! 0
