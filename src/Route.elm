module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), s)


type Route
    = Home
    | Group String

routeParser : Parser.Parser (Route -> a) a
routeParser =
  Parser.oneOf
    [ Parser.map Home Parser.top
    , Parser.map Group (Parser.top </> s "g" </> Parser.string)
    ]

fromUrl : Url -> Maybe Route
fromUrl = Parser.parse routeParser
