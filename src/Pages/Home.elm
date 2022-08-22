module Pages.Home exposing (Model, Msg, init, update, view)

import Html


type alias Model =
    {}


type alias Msg =
    {}


update : Msg -> Model -> (Msg -> msg) -> ( Model, Cmd msg )
update _ model _ =
    ( model, Cmd.none )


view : Model -> (Msg -> msg) -> Html.Html msg
view _ _ =
    Html.div [] []


init : (Msg -> msg) -> ( Model, Cmd msg )
init _ =
    ( {}, Cmd.none )
