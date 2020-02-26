module Pages.Home exposing (Model, Msg, init, update, view)

import Html.Styled as Html


type alias Model =
    {}


type alias Msg =
    {}


update : Msg -> Model -> (Msg -> msg) -> ( Model, Cmd msg )
update msg model toMsg =
    ( model, Cmd.none )


view : Model -> (Msg -> msg) -> Html.Html msg
view model toMsg =
    Html.div [] []


init : (Msg -> msg) -> ( Model, Cmd msg )
init toMsg =
    ( {}, Cmd.none )
