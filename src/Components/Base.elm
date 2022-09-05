module Components.Base exposing (view)

import Forms.Common exposing (mapClasses)
import Html
import Html.Attributes as Attrs


view : String -> List (Html.Html msg) -> Html.Html msg
view title body =
    Html.div []
        (List.append
            [ Html.object
                ([ Attrs.type_ "image/svg+xml"
                 , Attrs.attribute "data" "/static/images/github.svg"
                 ]
                    ++ mapClasses
                        [ "absolute"
                        , "top-0"
                        , "right-0"
                        , "w-16"
                        , "h-16"
                        ]
                )
                []
            , Html.h1
                []
                [ Html.text ("Fooskill -- " ++ title) ]
            ]
            body
        )
