module Route exposing (Route(..), createGroupUrl, createUserUrl, fromUrl)

import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), s)


baseUrl : String
baseUrl =
    ""


type Route
    = Home
    | Group { secretGroupId : String }
    | User
        { secretGroupId : String
        , userId : String
        }


createUserUrl : String -> String -> String
createUserUrl secretGroupId userId =
    Url.Builder.relative
        [ baseUrl
        , secretGroupId
        , "u"
        , userId
        ]
        []


createGroupUrl : String -> String
createGroupUrl secretGroupId =
    Url.Builder.relative
        [ baseUrl
        , secretGroupId
        ]
        []


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
    let
        basedParser parser =
            if String.isEmpty baseUrl then
                parser

            else
                s baseUrl </> parser
    in
    basedParser <|
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
