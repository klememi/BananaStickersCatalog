module Helpers where

data Country = Country { name :: String
                       , code :: String
                       }

newtype Brand = Brand String

data Page = Index
          | Countries
          | Brands

newtype User = User String

newtype State = State (Maybe User)

initial :: State
initial = State Nothing

countries :: [String]
countries = [ "Argentina",
              "Australia",
              "Bahamas",
              "Barbados",
              "Belize",
              "Bermuda",
              "Bolivia",
              "Brazil",
              "Cameroon",
              "Colombia",
              "Costa Rica",
              "Côte d’Ivoire",
              "Cuba",
              "Dominican Republic",
              "Ecuador",
              "El Salvador",
              "Equatorial Guinea",
              "Eritrea",
              "Ethiopia",
              "Fiji",
              "Gabon",
              "Gambia",
              "Ghana",
              "Guadeloupe",
              "Guatemala",
              "Guyana",
              "Haiti",
              "Honduras",
              "Indonesia",
              "Jamaica",
              "Madagascar",
              "Mali",
              "Martinique",
              "Mauritania",
              "Mauritius",
              "Mexico",
              "Morocco",
              "Mozambique",
              "New Zealand",
              "Nicaragua",
              "Nigeria",
              "Panama",
              "Papua New Guinea",
              "Paraguay",
              "Peru",
              "Philippines",
              "Puerto Rico",
              "Réunion",
              "Senegal",
              "Seychelles",
              "Singapore",
              "South Africa",
              "Sri Lanka",
              "Thailand",
              "Trinidad & Tobago",
              "Uruguay",
              "Venezuela"
            ]
