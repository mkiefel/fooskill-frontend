module Pages.Group exposing (Model, Msg, init, update, view)

import Api
import Color
import Css
import Forms.AddGame
import Forms.AddUser
import Forms.Common
import Html.Styled as Html
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
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
import LineChart.Colors
import LineChart.Container
import LineChart.Dots
import LineChart.Events
import LineChart.Grid
import LineChart.Interpolation
import LineChart.Junk
import LineChart.Legends
import LineChart.Line
import Url exposing (Url)
import Url.Builder


type alias Leaderboard =
    { users : List Api.User
    }


type alias Model =
    { leaderboard : Maybe Leaderboard
    , addGameFormState : Forms.AddGame.State
    , addUserFormState : Forms.AddUser.State
    , secretGroupId : String
    }


type Msg
    = GotLeaderboard (Result Http.Error Leaderboard)
    | GotAddGameFormMsg Forms.AddGame.Msg
    | GotAddUserFormMsg Forms.AddUser.Msg


init : String -> (Msg -> msg) -> ( Model, Cmd msg )
init secretGroupId toMsg =
    ( { leaderboard = Nothing
      , addGameFormState = Forms.AddGame.init secretGroupId
      , addUserFormState = Forms.AddUser.init secretGroupId
      , secretGroupId = secretGroupId
      }
    , requestLeaderboard secretGroupId toMsg
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
                    Forms.AddGame.update
                        reloadLeaderboard
                        subMsg
                        model.addGameFormState
                        (toMsg << GotAddGameFormMsg)
            in
            ( { model | addGameFormState = newState }, cmd )

        GotAddUserFormMsg subMsg ->
            let
                ( newState, cmd ) =
                    Forms.AddUser.update
                        subMsg
                        model.addUserFormState
                        (toMsg << GotAddUserFormMsg)
            in
            ( { model | addUserFormState = newState }, cmd )


view : Model -> (Msg -> msg) -> Html.Html msg
view model toMsg =
    let
        body =
            if String.isEmpty model.secretGroupId then
                []

            else
                [ Html.h2 [] [ Html.text "Leaderboard" ]
                , viewLeaderboard model.secretGroupId model.leaderboard
                , Html.h2 [] [ Html.text "Add game" ]
                , Forms.AddGame.view model.addGameFormState (toMsg << GotAddGameFormMsg)
                , Html.h2 [] [ Html.text "Add user" ]
                , Forms.AddUser.view model.addUserFormState (toMsg << GotAddUserFormMsg)
                ]
    in
    Html.div []
        (List.append
            [ Html.h1 [] [ Html.text "Fooskill" ] ]
            body
        )



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


viewUser : Float -> Float -> String -> Api.User -> Html.Html msg
viewUser minScore maxScore secretGroupId user =
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
            minScore - (maxScore - minScore) * 0.4

        paddedMaxScore =
            maxScore + (maxScore - minScore) * 0.2

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
                , line = LineChart.Line.wider 1
                , dots = LineChart.Dots.default
                }
                [ LineChart.line
                    (Color.rgb255 96 193 125)
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
        [ Attrs.css
            [ Css.position Css.relative ]
        ]
        [ Html.div
            [ Attrs.css
                [ Css.displayFlex
                , Css.justifyContent Css.spaceBetween
                ]
            ]
            [ Html.span []
                [ Html.a
                    [ Attrs.href
                        (Url.Builder.relative
                            [ secretGroupId, "users", user.id ]
                            []
                        )
                    ]
                    [ Html.text user.name ]
                , Html.span
                    [ Attrs.css
                        [ Css.fontSize (Css.em 0.7)
                        , Css.marginLeft (Css.px 5)
                        ]
                    ]
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
                ]
            , Html.span
                [ Attrs.css
                    [ Css.float Css.right
                    ]
                ]
                [ Html.text <| String.fromFloat <| toString 2 score ]
            ]
        , Html.div
            [ Attrs.css
                [ Css.position Css.absolute
                , Css.bottom Css.zero
                , Css.left Css.zero
                , Css.right Css.zero
                , Css.zIndex (Css.int -5)
                ]
            ]
            [ Html.fromUnstyled chart ]
        ]


viewLeaderboard : String -> Maybe Leaderboard -> Html.Html msg
viewLeaderboard secretGroupId maybeLeaderboard =
    case maybeLeaderboard of
        Just leaderboard ->
            let
                scores =
                    List.map scoreUser leaderboard.users

                minScore =
                    Maybe.withDefault 0 (List.minimum scores)

                maxScore =
                    Maybe.withDefault 100 (List.maximum scores)
            in
            Html.ol []
                (List.map
                    (viewUser minScore maxScore secretGroupId)
                    leaderboard.users
                )

        Nothing ->
            Html.p [] [ Html.text "loading..." ]
