module Main exposing (main)

import Browser
import Browser.Navigation
import Pages.Group
import Pages.Home
import Pages.User
import Route
import Url


main =
    Browser.application
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }



-- App


init : {} -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        route =
            Maybe.withDefault Route.Home (Route.fromUrl url)

        initWith subInit toMsg toModel =
            let
                ( subModel, cmd ) =
                    subInit toMsg
            in
            ( toModel subModel, cmd )
    in
    case route of
        Route.Home ->
            initWith Pages.Home.init GotHomeMsg Home

        Route.Group parameter ->
            initWith (Pages.Group.init parameter) GotGroupMsg Group

        Route.User parameter ->
            initWith (Pages.User.init parameter) GotUserMsg User


type Model
    = Home Pages.Home.Model
    | Group Pages.Group.Model
    | User Pages.User.Model


type Msg
    = GotHomeMsg Pages.Home.Msg
    | GotGroupMsg Pages.Group.Msg
    | GotUserMsg Pages.User.Msg
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotHomeMsg subMsg, Home subModel ) ->
            let
                ( newSubModel, cmd ) =
                    Pages.Home.update subMsg subModel GotHomeMsg
            in
            ( Home newSubModel, cmd )

        ( GotGroupMsg subMsg, Group subModel ) ->
            let
                ( newSubModel, cmd ) =
                    Pages.Group.update subMsg subModel GotGroupMsg
            in
            ( Group newSubModel, cmd )

        ( GotUserMsg subMsg, User subModel ) ->
            let
                ( newSubModel, cmd ) =
                    Pages.User.update subMsg subModel GotUserMsg
            in
            ( User newSubModel, cmd )

        ( OnUrlRequest request, _ ) ->
            case request of
                Browser.Internal url ->
                    ( model, Browser.Navigation.load (Url.toString url) )

                Browser.External url ->
                    ( model, Browser.Navigation.load url )

        ( OnUrlChange _, _ ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model of
                Home subModel ->
                    Pages.Home.view subModel GotHomeMsg

                Group subModel ->
                    Pages.Group.view subModel GotGroupMsg

                User subModel ->
                    Pages.User.view subModel GotUserMsg
    in
    { title = "Fooskill"
    , body = [ body ]
    }
