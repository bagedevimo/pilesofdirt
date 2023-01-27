module NES exposing (container, progress)

import Html exposing (Html)
import Html.Attributes as Attr


container : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
container title attrs children =
    Html.div (Attr.class "nes-container with-title" :: attrs) (Html.h3 [ Attr.class "title" ] [ Html.text title ] :: children)


progress : Float -> Html msg
progress value =
    Html.progress
        [ Attr.class "nes-progress", Attr.value (String.fromFloat value), Attr.max "100" ]
        []
