module Main exposing (..)

import Browser
import Html exposing (Html)
import Page.Question as Question
import Page.StartPage as StartPage


---- MODEL ----

type Model =
      StartPage String
    | Question Question.Model


init : {country: String, showDash: Bool} -> ( Model, Cmd Msg )
init flags = if flags.showDash
    then (StartPage flags.country, Cmd.none)
    else Question.init
        |> updateWith Question GotQuestionMsg


---- UPDATE ----

type Msg
    = StartGame
    | GotQuestionMsg Question.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case (model, msg) of
    (StartPage c, StartGame) ->
        Question.init
            |> updateWith Question GotQuestionMsg
    (Question question, GotQuestionMsg qMsg) ->
        Question.update qMsg question
            |> updateWith Question GotQuestionMsg
    (_,_) -> (model, Cmd.none)

updateWith : (subModel -> Model) ->
             (subMsg -> Msg) ->
             ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) = (toModel subModel, Cmd.map toMsg subCmd)


---- VIEW ----

view : Model -> Html Msg
view model = case model of
    StartPage _-> StartPage.view StartGame
    Question m -> Question.view m |> Html.map GotQuestionMsg


---- PROGRAM ----


main : Program {country: String, showDash: Bool} Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
