# Banana Sticker Catalog 

[![Build Status](https://travis-ci.org/klememi/BananaStickersCatalog.svg?branch=master)](https://travis-ci.org/klememi/BananaStickersCatalog)

If you start to collect things like banana stickers you might want to organize them well. This web application brings you opportunity to organize your collectibles and search in it simple and fast. Have you found a new sticker but you are not sure if you already have it in your collection? You can figure it out with this application simply.

*This project was made as a semestral project for Applied Functional Programming (MI-AFP) at FIT CTU in Prague.*

## Features

- showing collectibles from database
- adding new stickers
- showing detail info of stickers
- searching for sticker by attributes

## Possible improvements

- customization of collectibles
- visual customization
- security
- database export & import
- admin account

## Installation

1. This is Haskell Stack project so I recommend installing [Haskell Platform](https://www.haskell.org/downloads#platform). If you need just Stack follow [these](https://docs.haskellstack.org/en/stable/README/) instructions.
2. Clone this repository `git clone https://github.com/klememi/BananaStickersCatalog.git`
3. open project directory `cd BananaStickersCatalog`
4. build the project `stack build`
5. run tests `stack test`
6. generate documentation `stack haddock`
7. run the application `stack exec catalog`
8. open web browser `localhost:3000`

*Example SQLite database is included as* `stickers.db` *file. If you want to delete all records from the database run* `./resetDb.sh` *from the project directory (you need to have* `sqlite3` *installed).*

## Special thanks to

[<img src="https://bulma.io/images/made-with-bulma.png" width="128" height="24">](https://bulma.io)

[CSS flags](http://flag-icon-css.lip.is)

## License

[MIT License](https://choosealicense.com/licenses/mit/)
