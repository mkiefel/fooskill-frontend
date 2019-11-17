module Forms.Common exposing (viewInput, viewInputMessage, viewSubmitButton, viewSubmitMessage)

import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attrs


viewSubmitButton : String -> Bool -> Html.Html msg
viewSubmitButton text enabled =
    let
        enabledStyles =
            if enabled then
                [ Css.cursor Css.pointer
                , Css.boxShadow5
                    Css.zero
                    Css.zero
                    (Css.px 5)
                    Css.zero
                    (Css.rgba 0 0 0 0.25)
                ]

            else
                [ Css.cursor Css.notAllowed
                , Css.opacity (Css.num 0.5)
                ]
    in
    Html.button
        [ Attrs.type_ "submit"
        , Attrs.css
            (List.append
                [ Css.padding2 (Css.px 10) (Css.px 30)
                , Css.backgroundColor (Css.hex "60c17d")
                , Css.borderWidth (Css.px 1)
                , Css.borderColor Css.transparent
                , Css.borderRadius (Css.px 3)
                , Css.color (Css.hex "fff")
                , Css.marginRight (Css.px 5)
                , Css.active
                    [ Css.backgroundColor (Css.hex "408053")
                    ]
                , Css.pseudoClass "focus:not(:active)"
                    [ Css.boxShadow5
                        Css.zero
                        Css.zero
                        (Css.px 5)
                        Css.zero
                        (Css.rgba 72 199 116 0.8)
                    ]
                ]
                enabledStyles
            )
        ]
        [ Html.text text ]


viewSubmitMessageItems : String -> String -> List (Html.Html msg)
viewSubmitMessageItems text icon =
    [ Html.span
        [ Attrs.css
            [ Css.padding (Css.px 3) ]
        ]
        [ Html.i
            [ Attrs.class "fas"
            , Attrs.class icon
            , Attrs.css
                [ Css.marginLeft (Css.px 3)
                , Css.marginRight (Css.px 3)
                ]
            ]
            []
        , Html.text text
        ]
    ]


viewSubmitMessage :
    Maybe (Result String String)
    -> List (Html.Html msg)
viewSubmitMessage message =
    case message of
        Just result ->
            case result of
                Ok text ->
                    viewSubmitMessageItems text "fa-check-circle"

                Err text ->
                    viewSubmitMessageItems text "fa-times-circle"

        Nothing ->
            []


viewInputMessage : Maybe String -> List (Html.Html msg)
viewInputMessage message =
    case message of
        Just text ->
            [ Html.span
                [ Attrs.css
                    [ Css.position Css.absolute
                    , Css.bottom Css.zero
                    , Css.right Css.zero
                    , Css.color (Css.hex "f14668")
                    ]
                ]
                [ Html.text text ]
            ]

        Nothing ->
            []


viewInput label value attributes_label attributes_input chips menu =
    Html.label
        [ Attrs.css
            (List.append
                [ Css.display Css.block
                , Css.marginBottom (Css.px 10)
                ]
                attributes_label
            )
        ]
        [ Html.text label
        , Html.div
            [ Attrs.css
                [ Css.displayFlex
                , Css.border3 (Css.px 1) Css.solid (Css.hex "60c17d")
                , Css.borderRadius (Css.px 3)
                , Css.padding (Css.px 10)
                , Css.alignItems Css.center
                ]
            ]
            (Html.span
                [ Attrs.css
                    [ Css.position Css.relative
                    , Css.order (Css.num 1)
                    , Css.flexGrow (Css.num 1)
                    ]
                ]
                (List.append
                    [ Html.input
                        (List.append
                            [ Attrs.css
                                [ Css.border Css.zero
                                , Css.width (Css.pct 100)
                                ]
                            , Attrs.value value
                            ]
                            attributes_input
                        )
                        []
                    ]
                    menu
                )
                :: chips
            )
        ]
