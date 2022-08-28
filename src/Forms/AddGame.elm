module Forms.AddGame exposing (Msg, State, init, update, view)

import Api
import Components.Chips as Chips
import Forms.Common as Common
import Html
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder


type alias State =
    { winners : Chips.State
    , losers : Chips.State
    , message : Maybe (Result String String)
    , waiting : Bool
    , secretGroupId : String
    }


type alias RequestGame =
    { winner_ids : List String
    , loser_ids : List String
    }


encodeRequestGame : RequestGame -> Encode.Value
encodeRequestGame game =
    Encode.object
        [ ( "winner_ids", Encode.list Encode.string game.winner_ids )
        , ( "loser_ids", Encode.list Encode.string game.loser_ids )
        ]


init : String -> State
init secretGroupId =
    { winners = Chips.init secretGroupId
    , losers = Chips.init secretGroupId
    , message = Nothing
    , waiting = False
    , secretGroupId = secretGroupId
    }


type alias ResponseGame =
    { id : String
    , winner_ids : List String
    , loser_ids : List String
    }


type alias PostGame =
    { game : ResponseGame }


decodeResponseGame : Decode.Decoder ResponseGame
decodeResponseGame =
    Decode.map3
        ResponseGame
        (Decode.field "id" Decode.string)
        (Decode.field "winner_ids" <| Decode.list Decode.string)
        (Decode.field "loser_ids" <| Decode.list Decode.string)


decodePostGame : Decode.Decoder PostGame
decodePostGame =
    Decode.map
        PostGame
        (Decode.field "game" decodeResponseGame)


type Msg
    = GotWinnersMsg Chips.Msg
    | GotLosersMsg Chips.Msg
    | Submit
    | GotPostGame (Result Http.Error PostGame)


update : Cmd msg -> Msg -> State -> (Msg -> msg) -> ( State, Cmd msg )
update reload msg state toMsg =
    case msg of
        GotWinnersMsg subMsg ->
            let
                ( newWinners, cmd ) =
                    Chips.update subMsg state.winners (toMsg << GotWinnersMsg)
            in
            ( { state
                | winners = newWinners
                , message = Nothing
              }
            , cmd
            )

        GotLosersMsg subMsg ->
            let
                ( newLosers, cmd ) =
                    Chips.update subMsg state.losers (toMsg << GotLosersMsg)
            in
            ( { state
                | losers = newLosers
                , message = Nothing
              }
            , cmd
            )

        GotPostGame result ->
            case result of
                Ok _ ->
                    ( { state
                        | winners = Chips.init state.secretGroupId
                        , losers = Chips.init state.secretGroupId
                        , message = Just (Ok "game added")
                        , waiting = False
                      }
                    , reload
                    )

                Err _ ->
                    ( { state
                        | message = Just (Err "something went wrong")
                        , waiting = False
                      }
                    , Cmd.none
                    )

        Submit ->
            if state.waiting then
                ( state, Cmd.none )

            else
                submit state toMsg


checkInput : Chips.State -> Bool
checkInput state =
    not (List.isEmpty (Chips.getUsers state)) && String.isEmpty (Chips.getQuery state)


submit : State -> (Msg -> msg) -> ( State, Cmd msg )
submit state toMsg =
    let
        selectText game input_state =
            case game of
                Just _ ->
                    Nothing

                Nothing ->
                    if checkInput input_state then
                        Nothing

                    else
                        Just "your pick here?"

        newWinnersMessage =
            selectText maybeGame state.winners

        newLosersMessage =
            selectText maybeGame state.losers

        maybeGame =
            let
                allClean =
                    checkInput state.winners
                        && checkInput state.losers

                winner_ids =
                    List.map .id (Chips.getUsers state.winners)

                loser_ids =
                    List.map .id (Chips.getUsers state.losers)
            in
            if
                not (List.isEmpty winner_ids)
                    && not (List.isEmpty loser_ids)
                    && allClean
            then
                Just
                    { winner_ids = winner_ids
                    , loser_ids = loser_ids
                    }

            else
                Nothing

        submitCmd =
            case maybeGame of
                Just game ->
                    Http.post
                        { url = Url.Builder.relative [ Api.apiUrl, state.secretGroupId, "games" ] []
                        , expect = Http.expectJson (toMsg << GotPostGame) decodePostGame
                        , body = Http.jsonBody (encodeRequestGame game)
                        }

                Nothing ->
                    Cmd.none
    in
    ( { state
        | winners = Chips.setMessage newWinnersMessage state.winners
        , losers = Chips.setMessage newLosersMessage state.losers
        , waiting = Maybe.withDefault False (Maybe.map (\_ -> True) maybeGame)
      }
    , submitCmd
    )


view : State -> (Msg -> msg) -> Html.Html msg
view state toMsg =
    Html.form [ Events.onSubmit (toMsg Submit) ]
        (List.append
            [ Chips.view
                "winners"
                state.winners
                (toMsg << GotWinnersMsg)
            , Chips.view
                "losers"
                state.losers
                (toMsg << GotLosersMsg)
            , Common.viewSubmitButton "add game!" (not state.waiting)
            ]
            (Common.viewSubmitMessage state.message)
        )
