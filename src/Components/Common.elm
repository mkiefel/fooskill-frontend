module Components.Common exposing
    ( mapClasses
    , viewInput
    , viewInputMessage
    , viewSection
    , viewSubmitButton
    , viewSubmitMessage
    , viewUserLink
    )

import Html
import Html.Attributes as Attrs
import Route


mapClasses : List String -> List (Html.Attribute msg)
mapClasses =
    List.map Attrs.class


viewSubmitButton : String -> Bool -> Html.Html msg
viewSubmitButton text enabled =
    Html.button
        (mapClasses
            [ "bg-gradient-to-r"
            , "from-green-400"
            , "to-blue-500"
            , "hover:from-green-500"
            , "hover:to-blue-600"
            , "px-5"
            , "py-2.5"
            , "text-base"
            , "leading-5"
            , "rounded-md"
            , "font-semibold"
            , "text-white"
            , "disabled:bg-slate-500"
            , "drop-shadow-md"
            ]
            ++ [ Attrs.type_ "submit"
               , Attrs.disabled (not enabled)
               ]
        )
        [ Html.text text ]


viewSubmitMessageItems : String -> String -> List (Html.Html msg)
viewSubmitMessageItems text icon =
    [ Html.span
        [ Attrs.class "p-1" ]
        [ Html.i
            (mapClasses [ "fas", icon, "ml-1", "mr-1" ])
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
                (mapClasses [ "absolute", "bottom-0", "right-0", "text-red-500", "py-1" ])
                [ Html.text text ]
            ]

        Nothing ->
            []


viewInput : String -> String -> List (Html.Attribute msg) -> List (Html.Attribute msg) -> List (Html.Html msg) -> List (Html.Html msg) -> Html.Html msg
viewInput label value attributes_label attributes_input chips menu =
    let
        classes =
            mapClasses
                [ "mt-1"
                , "w-full"
                , "px-1.5"
                , "py-1"

                -- Appearence.
                , "bg-white"
                , "border"
                , "border-green-400"
                , "rounded-md"
                , "text-sm"
                , "shadow-sm"

                -- Items will get ordered inside according to flex.
                , "flex"
                , "items-center"
                ]
    in
    Html.label
        (mapClasses [ "block", "mb-1" ]
            ++ attributes_label
        )
        [ Html.text label
        , Html.div
            classes
            (Html.span
                (mapClasses [ "relative", "order-1", "grow" ])
                (List.append
                    [ Html.input
                        (mapClasses
                            [ "border-0"
                            , "w-full"
                            , "outline-none"
                            , "py-1"
                            ]
                            ++ (Attrs.value value
                                    :: attributes_input
                               )
                        )
                        []
                    ]
                    menu
                )
                :: chips
            )
        ]


viewSection : List (Html.Html msg) -> Html.Html msg
viewSection title =
    Html.h2 (mapClasses [ "relative" ])
        [ Html.span [] title
        , Html.span
            (mapClasses
                [ "absolute"
                , "bottom-0"
                , "w-full"
                , "h-1.5"
                , "left-0"
                , "bg-green-400"
                , "-z-10"
                ]
            )
            []
        ]


viewUserLink : String -> String -> String -> Html.Html msg
viewUserLink secretGroupId userId userName =
    Html.a
        [ Attrs.href (Route.createUserUrl secretGroupId userId) ]
        [ Html.span (mapClasses [ "relative" ])
            [ Html.text userName
            , Html.span
                (mapClasses
                    [ "absolute"
                    , "bottom-[0.12rem]"
                    , "w-full"
                    , "h-[0.08rem]"
                    , "left-0"
                    , "bg-gradient-to-r"
                    , "from-green-400"
                    , "to-blue-500"
                    , "-z-10"
                    ]
                )
                []
            ]
        ]
