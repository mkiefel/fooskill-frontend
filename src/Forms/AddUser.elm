module Forms.AddUser exposing (Msg, State, init, update, view)

import Api
import Forms.Common as Common
import Html
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder


type alias State =
    { user : String
    , inputMessage : Maybe String
    , message : Maybe (Result String String)
    , waiting : Bool
    , secretGroupId : String
    }


init : String -> State
init secretGroupId =
    { user = ""
    , inputMessage = Nothing
    , message = Nothing
    , waiting = False
    , secretGroupId = secretGroupId
    }


type Msg
    = SetUser String
    | Submit
    | GotPostUser (Result Http.Error PostUser)


type alias PostUser =
    { user : Api.User }


postUserDecoder : Decode.Decoder PostUser
postUserDecoder =
    Decode.map PostUser <| Decode.field "user" Api.userDecoder


update : Msg -> State -> (Msg -> msg) -> ( State, Cmd msg )
update msg state toMsg =
    case msg of
        SetUser value ->
            ( { state | user = value }, Cmd.none )

        GotPostUser result ->
            case result of
                Ok postUser ->
                    ( { state
                        | message = Just (Ok ("user '" ++ postUser.user.name ++ "' created"))
                        , inputMessage = Nothing
                        , waiting = False
                        , user = ""
                      }
                    , Cmd.none
                    )

                Err (Http.BadStatus 409) ->
                    ( { state
                        | message = Nothing
                        , inputMessage = Just "user already exists"
                        , waiting = False
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { state
                        | message = Just (Err "something went wrong")
                        , inputMessage = Nothing
                        , waiting = False
                      }
                    , Cmd.none
                    )

        Submit ->
            if state.waiting then
                ( state, Cmd.none )

            else
                submit state toMsg


submit : State -> (Msg -> msg) -> ( State, Cmd msg )
submit state toMsg =
    if String.length state.user < 3 then
        ( { state
            | message = Nothing
            , inputMessage = Just "too short"
          }
        , Cmd.none
        )

    else
        ( { state
            | waiting = True
          }
        , Http.post
            { url =
                Url.Builder.relative
                    [ Api.apiUrl
                    , state.secretGroupId
                    , "users"
                    ]
                    []
            , expect =
                Http.expectJson
                    (toMsg << GotPostUser)
                    postUserDecoder
            , body =
                Http.jsonBody
                    (Encode.object
                        [ ( "name", Encode.string state.user ) ]
                    )
            }
        )


view : State -> (Msg -> msg) -> Html.Html msg
view state toMsg =
    Html.form [ Events.onSubmit (toMsg Submit) ]
        (List.append
            [ Common.viewInput
                "user"
                state.user
                []
                [ Events.onInput (toMsg << SetUser) ]
                []
                (Common.viewInputMessage state.inputMessage)
            , Common.viewSubmitButton "add user!" (not state.waiting)
            ]
            (Common.viewSubmitMessage state.message)
        )
