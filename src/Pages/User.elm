module Pages.User exposing (Model, Msg, init, update, view)

import Api
import Components.Base
import Forms.Common exposing (mapClasses)
import Html
import Html.Attributes as Attrs
import Http
import Json.Decode as Decode
import Url.Builder


type alias User =
    { user : Api.User
    , games : List Api.Game
    }


type alias Model =
    { user : Maybe User
    , secretGroupId : String
    , userId : String
    }


type Msg
    = GotUser (Result Http.Error User)


type alias Parameter =
    { secretGroupId : String
    , userId : String
    }


init : Parameter -> (Msg -> msg) -> ( Model, Cmd msg )
init parameter toMsg =
    ( { user = Nothing
      , secretGroupId = parameter.secretGroupId
      , userId = parameter.userId
      }
    , requestUser parameter.secretGroupId parameter.userId toMsg
    )


update : Msg -> Model -> (Msg -> msg) -> ( Model, Cmd msg )
update msg model _ =
    case msg of
        GotUser result ->
            case result of
                Ok user ->
                    ( { model | user = Just user }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> (Msg -> msg) -> Html.Html msg
view model _ =
    let
        toString precision f =
            let
                scale =
                    10 ^ precision
            in
            (toFloat <| round <| f * scale) / scale

        ( title, body ) =
            case model.user of
                Nothing ->
                    ( "User", [] )

                Just user ->
                    ( user.user.name
                    , [ Html.h2 []
                            [ Html.text "Score"
                            , Html.span
                                (mapClasses [ "text-sm", "ml-0.5" ])
                                [ Html.text
                                    (String.concat
                                        [ "μ = "
                                        , String.fromFloat
                                            (toString 2 user.user.player.skill.mu)
                                        , " σ² = "
                                        , String.fromFloat
                                            (toString 2 user.user.player.skill.sigma2)
                                        ]
                                    )
                                ]
                            ]
                      , Html.h2 [] [ Html.text "Games" ]
                      , viewGames model.secretGroupId user.games
                      ]
                    )
    in
    Components.Base.view title body



-- User


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2
        User
        (Decode.field "user" Api.userDecoder)
        (Decode.field "games" <| Decode.list Api.gameDecoder)


requestUser : String -> String -> (Msg -> msg) -> Cmd msg
requestUser secretGroupId userId toMsg =
    Http.get
        { url =
            Url.Builder.relative
                [ Api.apiUrl
                , secretGroupId
                , "users"
                , userId
                , "games"
                ]
                []
        , expect = Http.expectJson (toMsg << GotUser) userDecoder
        }


viewGames : String -> List Api.Game -> Html.Html msg
viewGames secretGroupId games =
    let
        viewRow : List (Html.Attribute msg) -> List (Html.Html msg) -> List (Html.Html msg) -> Html.Html msg
        viewRow classes left right =
            Html.li (mapClasses [ "block" ] ++ classes)
                [ Html.div (mapClasses [ "flex", "justify-between" ])
                    [ Html.span (mapClasses [ "flex", "space-x-1" ]) left
                    , Html.span (mapClasses [ "flex", "space-x-1" ]) right
                    ]
                ]

        viewPlayer : Api.User -> Html.Html msg
        viewPlayer player =
            Html.a
                [ Attrs.href
                    (Url.Builder.absolute
                        [ secretGroupId, "u", player.id ]
                        []
                    )
                ]
                [ Html.text player.name ]

        viewGame : Api.Game -> Html.Html msg
        viewGame game =
            viewRow []
                (List.map
                    viewPlayer
                    game.winners
                )
                (List.map
                    viewPlayer
                    game.losers
                )
    in
    Html.ol
        (mapClasses [ "list-none", "p-0" ])
        (viewRow (mapClasses [ "border-slate-400", "border-b" ])
            [ Html.text "winners" ]
            [ Html.text "losers" ]
            :: List.map viewGame games
        )
