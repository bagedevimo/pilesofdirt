module NES exposing (container)

import Html exposing (Html)
import Html.Attributes as Attr


container : String -> List (Html msg) -> Html msg
container title children =
    Html.div [ Attr.class "nes-container with-title" ] (Html.h3 [ Attr.class "title" ] [ Html.text title ] :: children)
