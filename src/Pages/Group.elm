module Pages.Group exposing (Model, Msg, init, update, view)

import Api
import Color
import Components.AddGame
import Components.AddUser
import Components.Base
import Components.Common exposing (mapClasses)
import Html
import Http
import Json.Decode as Decode
import LineChart
import LineChart.Area
import LineChart.Axis
import LineChart.Axis.Intersection
import LineChart.Axis.Line
import LineChart.Axis.Range
import LineChart.Axis.Ticks
import LineChart.Axis.Title
import LineChart.Container
import LineChart.Dots
import LineChart.Events
import LineChart.Grid
import LineChart.Interpolation
import LineChart.Junk
import LineChart.Legends
import LineChart.Line
import Url.Builder


type alias Leaderboard =
    { users : List Api.User
    }


type alias Model =
    { leaderboard : Maybe Leaderboard
    , addGameFormState : Components.AddGame.State
    , addUserFormState : Components.AddUser.State
    , secretGroupId : String
    }


type Msg
    = GotLeaderboard (Result Http.Error Leaderboard)
    | GotAddGameFormMsg Components.AddGame.Msg
    | GotAddUserFormMsg Components.AddUser.Msg


type alias Parameter =
    { secretGroupId : String }


init : Parameter -> (Msg -> msg) -> ( Model, Cmd msg )
init parameter toMsg =
    ( { leaderboard = Nothing
      , addGameFormState = Components.AddGame.init parameter.secretGroupId
      , addUserFormState = Components.AddUser.init parameter.secretGroupId
      , secretGroupId = parameter.secretGroupId
      }
    , requestLeaderboard parameter.secretGroupId toMsg
    )


update : Msg -> Model -> (Msg -> msg) -> ( Model, Cmd msg )
update msg model toMsg =
    case msg of
        GotLeaderboard result ->
            case result of
                Ok leaderboard ->
                    ( { model | leaderboard = Just leaderboard }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotAddGameFormMsg subMsg ->
            let
                reloadLeaderboard =
                    requestLeaderboard model.secretGroupId toMsg

                ( newState, cmd ) =
                    Components.AddGame.update
                        reloadLeaderboard
                        subMsg
                        model.addGameFormState
                        (toMsg << GotAddGameFormMsg)
            in
            ( { model | addGameFormState = newState }, cmd )

        GotAddUserFormMsg subMsg ->
            let
                ( newState, cmd ) =
                    Components.AddUser.update
                        subMsg
                        model.addUserFormState
                        (toMsg << GotAddUserFormMsg)
            in
            ( { model | addUserFormState = newState }, cmd )


view : Model -> (Msg -> msg) -> Html.Html msg
view model toMsg =
    let
        body =
            case ( model.secretGroupId, model.leaderboard ) of
                ( "", _ ) ->
                    []

                ( _, Nothing ) ->
                    []

                ( _, Just leaderboard ) ->
                    [ Components.Common.viewSection [ Html.text "Leaderboard" ]
                    , viewLeaderboard model.secretGroupId leaderboard
                    , Components.Common.viewSection [ Html.text "Add game" ]
                    , Components.AddGame.view model.addGameFormState (toMsg << GotAddGameFormMsg)
                    , Components.Common.viewSection [ Html.text "Add user" ]
                    , Components.AddUser.view model.addUserFormState (toMsg << GotAddUserFormMsg)
                    ]
    in
    Components.Base.view model.secretGroupId "Group" body



-- Leaderboard


leaderboardDecoder : Decode.Decoder Leaderboard
leaderboardDecoder =
    Decode.map Leaderboard <| Decode.field "users" <| Decode.list Api.userDecoder


requestLeaderboard : String -> (Msg -> msg) -> Cmd msg
requestLeaderboard secretGroupId toMsg =
    Http.get
        { url = Url.Builder.relative [ Api.apiUrl, secretGroupId, "leaderboard" ] []
        , expect = Http.expectJson (toMsg << GotLeaderboard) leaderboardDecoder
        }


type alias Point =
    { x : Float, y : Float }


scoreUser : Api.User -> Float
scoreUser user =
    user.player.skill.mu - 2.0 * sqrt user.player.skill.sigma2


viewUser : Float -> Float -> String -> Int -> Api.User -> Html.Html msg
viewUser minScore maxScore secretGroupId index user =
    let
        score =
            scoreUser user

        toString precision f =
            let
                scale =
                    10 ^ precision
            in
            (toFloat <| round <| f * scale) / scale

        density x =
            e
                ^ (-0.5
                    * (x - user.player.skill.mu)
                    ^ 2
                    / user.player.skill.sigma2
                  )
                / sqrt (2 * pi * user.player.skill.sigma2)

        paddedMinScore =
            minScore - (maxScore - minScore) * 0.5

        paddedMaxScore =
            maxScore + (maxScore - minScore) * 0.5

        samples =
            80

        data =
            List.map (\x -> Point x (density x)) <|
                List.map
                    (\i ->
                        toFloat i
                            / toFloat samples
                            * (paddedMaxScore - paddedMinScore)
                            + paddedMinScore
                    )
                <|
                    List.range 0 samples

        unpaddedAxis length accessor =
            LineChart.Axis.custom
                { title = LineChart.Axis.Title.default ""
                , variable = Just << accessor
                , pixels = length
                , range = LineChart.Axis.Range.padded 1 1
                , axisLine = LineChart.Axis.Line.none
                , ticks = LineChart.Axis.Ticks.custom (\_ _ -> [])
                }

        chart =
            LineChart.viewCustom
                { x = unpaddedAxis 600 .x
                , y = unpaddedAxis 50 .y
                , container =
                    LineChart.Container.custom
                        { attributesHtml = []
                        , attributesSvg = []
                        , size = LineChart.Container.relative
                        , margin = LineChart.Container.Margin 0 0 0 0
                        , id = user.id
                        }
                , interpolation = LineChart.Interpolation.default
                , intersection = LineChart.Axis.Intersection.default
                , legends = LineChart.Legends.none
                , events = LineChart.Events.default
                , junk = LineChart.Junk.default
                , grid = LineChart.Grid.default
                , area = LineChart.Area.default
                , line = LineChart.Line.default
                , dots = LineChart.Dots.default
                }
                [ LineChart.line
                    (Color.rgb255 74 222 128)
                    LineChart.Dots.none
                    "density"
                    data
                , LineChart.line
                    (Color.rgb255 241 70 104)
                    LineChart.Dots.circle
                    "score"
                    [ Point score (density score) ]
                ]
    in
    Html.li
        (mapClasses [ "relative" ])
        [ Html.div (mapClasses [ "flex" ])
            [ Html.span
                (mapClasses
                    [ "w-7"
                    , "mr-0.5"
                    , "text-right"
                    , "inline-block"
                    ]
                )
                [ Html.text (String.fromInt (index + 1) ++ ".") ]
            , Html.div
                (mapClasses [ "flex", "flex-wrap", "items-baseline", "grow" ])
                [ Html.span (mapClasses [ "mr-0.5" ])
                    [ Components.Common.viewUserLink secretGroupId user.id user.name ]
                , Html.span
                    (mapClasses [ "text-xs" ])
                    [ Html.text
                        (String.concat
                            [ "μ = "
                            , String.fromFloat
                                (toString 2 user.player.skill.mu)
                            , " σ² = "
                            , String.fromFloat
                                (toString 2 user.player.skill.sigma2)
                            ]
                        )
                    ]
                , Html.span
                    (mapClasses [ "ml-auto" ])
                    [ Html.text <| String.fromFloat <| toString 2 score ]
                ]
            , Html.div
                (mapClasses
                    [ "absolute"
                    , "bottom-0"
                    , "left-2"
                    , "right-0"
                    , "-z-20"
                    , "text-xs"
                    ]
                )
                [ chart ]
            ]
        ]


viewLeaderboard : String -> Leaderboard -> Html.Html msg
viewLeaderboard secretGroupId leaderboard =
    let
        scores =
            List.map scoreUser leaderboard.users

        minScore =
            Maybe.withDefault 0 (List.minimum scores)

        maxScore =
            Maybe.withDefault 100 (List.maximum scores)
    in
    Html.ol
        (mapClasses [ "list-none", "p-0" ])
        (List.indexedMap
            (viewUser minScore maxScore secretGroupId)
            leaderboard.users
        )
