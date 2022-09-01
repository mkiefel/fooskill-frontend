module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), s)


type Route
    = Home
    | Group { secretGroupId : String }
    | User
        { secretGroupId : String
        , userId : String
        }


createGroupRoute : String -> Route
createGroupRoute secretGroupId =
    Group { secretGroupId = secretGroupId }


createUserRoute : String -> String -> Route
createUserRoute secretGroupId userId =
    User
        { secretGroupId = secretGroupId
        , userId = userId
        }


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map createGroupRoute Parser.string
        , Parser.map createUserRoute
            (Parser.string
                </> s "u"
                </> Parser.string
            )
        ]


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse routeParser
