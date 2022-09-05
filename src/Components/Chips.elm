module Components.Chips exposing
    ( Msg
    , State
    , getQuery
    , getUsers
    , init
    , setMessage
    , update
    , view
    )

import Api
import Forms.Common exposing (mapClasses)
import Html
import Html.Events as Events
import Http
import Json.Decode as Decode
import Process
import Task
import Url.Builder



-- JSON


type alias QueryResponse =
    { query : String
    , users : List Api.User
    }


decodeQueryResponse : Decode.Decoder QueryResponse
decodeQueryResponse =
    Decode.map2 QueryResponse (Decode.field "query" Decode.string) <|
        Decode.field "users" <|
            Decode.list Api.userDecoder



-- Input


type alias State =
    { query : String
    , message : Maybe String
    , isFocused : Bool
    , options : List Api.User
    , selectedOption : Maybe Api.User
    , chips : List Api.User
    , secretGroupId : String
    }


getUsers : State -> List Api.User
getUsers state =
    state.chips


getQuery : State -> String
getQuery state =
    state.query


setMessage : Maybe String -> State -> State
setMessage message state =
    { state | message = message }


init : String -> State
init secretGroupId =
    { query = ""
    , message = Nothing
    , isFocused = False
    , options = []
    , selectedOption = Nothing
    , chips = []
    , secretGroupId = secretGroupId
    }


type Msg
    = SetQuery String
    | OnQueryResponse (Result Http.Error QueryResponse)
    | HandleUp
    | HandleDown
    | RemoveChip
    | AddChip
    | OnBlur
    | OnSelect Api.User


update : Msg -> State -> (Msg -> msg) -> ( State, Cmd msg )
update msg state toMsg =
    case msg of
        SetQuery newQuery ->
            let
                minimumQueryLetters =
                    3

                maybeBackEndQuery =
                    let
                        doQueryBackend =
                            state.query
                                /= newQuery
                                && String.length newQuery
                                >= minimumQueryLetters
                    in
                    if doQueryBackend then
                        Http.get
                            { url =
                                Url.Builder.relative
                                    [ Api.apiUrl
                                    , state.secretGroupId
                                    , "users"
                                    ]
                                    [ Url.Builder.string
                                        "query"
                                        newQuery
                                    ]
                            , expect =
                                Http.expectJson
                                    (toMsg << OnQueryResponse)
                                    decodeQueryResponse
                            }

                    else
                        Cmd.none

                hasQueryChanged =
                    not (state.query == newQuery)

                -- Clear message if query has changed.
                newMessage =
                    if hasQueryChanged then
                        Nothing

                    else
                        state.message

                newSelectedOption =
                    List.head state.options
            in
            ( { state
                | query = newQuery
                , message = newMessage
                , selectedOption = newSelectedOption
              }
            , maybeBackEndQuery
            )

        OnQueryResponse result ->
            case result of
                Ok queryResponse ->
                    if queryResponse.query == state.query then
                        update
                            (SetQuery state.query)
                            { state | options = queryResponse.users }
                            toMsg

                    else
                        ( state, Cmd.none )

                Err _ ->
                    ( state, Cmd.none )

        HandleUp ->
            case state.selectedOption of
                Nothing ->
                    ( state, Cmd.none )

                Just option ->
                    let
                        newSelectedOption =
                            Just (selectPrev state.options .id option)
                    in
                    ( { state
                        | selectedOption =
                            newSelectedOption
                      }
                    , Cmd.none
                    )

        HandleDown ->
            case state.selectedOption of
                Nothing ->
                    ( state, Cmd.none )

                Just option ->
                    let
                        newSelectedOption =
                            Just (selectNext state.options .id option)
                    in
                    ( { state
                        | selectedOption =
                            newSelectedOption
                      }
                    , Cmd.none
                    )

        OnSelect option ->
            ( { state | selectedOption = Just option }, Cmd.none )

        AddChip ->
            addChip state

        RemoveChip ->
            ( { state
                | chips =
                    List.reverse <|
                        Maybe.withDefault [] <|
                            List.tail <|
                                List.reverse state.chips
              }
            , Cmd.none
            )

        OnBlur ->
            ( state
            , Process.sleep 100
                |> Task.perform (always (toMsg AddChip))
            )


addChip : State -> ( State, Cmd msg )
addChip state =
    case state.selectedOption of
        Nothing ->
            ( state, Cmd.none )

        Just option ->
            let
                newChips =
                    List.append state.chips [ option ]
            in
            ( { state
                | selectedOption = Nothing
                , query = ""
                , options = []
                , chips = newChips
              }
            , Cmd.none
            )


selectNext : List a -> (a -> b) -> a -> a
selectNext items toKey selectedItem =
    case items of
        item :: rest ->
            if toKey item == toKey selectedItem then
                Maybe.withDefault selectedItem (List.head rest)

            else
                selectNext rest toKey selectedItem

        [] ->
            selectedItem


selectPrev : List a -> (a -> b) -> a -> a
selectPrev items toKey selectedItem =
    selectNext (List.reverse items) toKey selectedItem


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


view : String -> State -> (Msg -> msg) -> Html.Html msg
view label state toMsg =
    let
        mapKeycode : Int -> Decode.Decoder ( msg, Bool )
        mapKeycode code =
            if code == 38 then
                Decode.succeed ( toMsg HandleUp, True )

            else if code == 40 then
                Decode.succeed ( toMsg HandleDown, True )

            else if code == 8 && state.query == "" then
                Decode.succeed ( toMsg RemoveChip, True )

            else if code == 13 && isJust state.selectedOption then
                Decode.succeed ( toMsg AddChip, True )

            else
                Decode.fail "unhandled key"

        keydownDecoder : Decode.Decoder ( msg, Bool )
        keydownDecoder =
            Events.keyCode
                |> Decode.andThen mapKeycode

        chips =
            List.map viewChip state.chips
    in
    Forms.Common.viewInput
        label
        state.query
        []
        [ Events.onInput (toMsg << SetQuery)
        , Events.onBlur (toMsg OnBlur)
        , Events.preventDefaultOn "keydown" keydownDecoder
        ]
        chips
        (List.append
            (viewMenu state toMsg)
            (Forms.Common.viewInputMessage state.message)
        )


viewChip : Api.User -> Html.Html msg
viewChip chip =
    Html.span
        (mapClasses
            [ "bg-green-400"
            , "text-white"
            , "rounded-full"
            , "mr-1"
            , "px-3"

            -- Should match the classes of the input.
            , "py-1"
            ]
        )
        [ Html.text chip.name ]


viewOption : Api.User -> (Msg -> msg) -> Api.User -> Html.Html msg
viewOption selectedOption toMsg option =
    let
        selectedCss =
            if option.id == selectedOption.id then
                mapClasses [ "bg-green-400", "text-white" ]

            else
                []
    in
    Html.li
        (Events.onMouseDown (toMsg (OnSelect option))
            :: selectedCss
            ++ mapClasses
                [ "block"
                , "px-1.5"
                , "py-1"
                , "cursor-pointer"
                ]
        )
        [ Html.text option.name
        ]


viewMenu : State -> (Msg -> msg) -> List (Html.Html msg)
viewMenu state toMsg =
    case state.selectedOption of
        Nothing ->
            []

        Just option ->
            let
                options =
                    List.map
                        (viewOption option toMsg)
                        state.options
            in
            [ Html.div
                (mapClasses
                    [ "absolute"
                    , "bg-white"
                    , "border"
                    , "border-slate-300"
                    , "drop-shadow-md"
                    , "z-10"
                    , "top-full"
                    , "left-0"
                    , "rounded-md"
                    , "overflow-hidden"
                    ]
                )
                [ Html.ul
                    (mapClasses
                        [ "list-none"
                        , "p-0"
                        , "m-auto"
                        , "max-h-48"
                        , "overflow-y-auto"
                        ]
                    )
                    options
                ]
            ]
