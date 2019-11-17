module Pages.Group exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Css
import Forms.AddGame
import Forms.AddUser
import Forms.Common
import Html.Styled as Html
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Http
import Json.Decode as Decode
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


subscriptions : (Msg -> msg) -> Sub msg
subscriptions toMsg =
    Sub.batch
        [ Forms.AddGame.subscriptions (toMsg << GotAddGameFormMsg) ]


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


viewUser : String -> Api.User -> Html.Html msg
viewUser secretGroupId user =
    let
        score =
            user.player.skill.mu - 2.0 * sqrt user.player.skill.sigma2

        toString precision f =
            let
                scale =
                    10 ^ precision
            in
            (toFloat <| round <| f * scale) / scale
    in
    Html.li []
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
        ]


viewLeaderboard : String -> Maybe Leaderboard -> Html.Html msg
viewLeaderboard secretGroupId maybeLeaderboard =
    case maybeLeaderboard of
        Just leaderboard ->
            Html.ol [] (List.map (viewUser secretGroupId) leaderboard.users)

        Nothing ->
            Html.p [] [ Html.text "loading..." ]
