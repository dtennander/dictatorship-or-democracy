module Page.StartPage exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)

-- View

view : msg -> Html msg
view onStart =
    div [] [
         h1 [] [ text "Welcome to Dem or Dict!"],
         p [] [ text "Please feel free to start the game!" ],
         button [onClick onStart] [ text "Start Game!"]
    ]
