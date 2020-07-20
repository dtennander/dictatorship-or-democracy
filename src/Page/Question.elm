module Page.Question exposing (view, Model, init, update, Msg)

import API.Countries exposing (CountryCode, Time(..), getCountryName, getGDP, getRuralPop, parseCountryCode)
import Debug exposing (toString)
import Html exposing (..)
import Http
import String exposing (fromFloat, fromInt)

-- MODEL
type alias Model = {
        code: Maybe CountryCode,
        name: Maybe String,
        gdp: Maybe (Year, Float),
        electricity: Maybe (Year, Float),
        lastError: Maybe Http.Error
    }

init: String -> (Model, Cmd Msg)
init cc =
    let baseModel = {name = Nothing, gdp = Nothing, code = Nothing, lastError = Nothing, electricity = Nothing}
    in case parseCountryCode cc of
        Just c -> ({baseModel | code = Just c}, Cmd.batch [fetchCountryName c, fetchCountryGDP c, fetchElectricity c])
        Nothing -> (baseModel, Cmd.none)


-- UPDATE
type Msg =
      FailedToFetch Http.Error
    | FetchedName String
    | FetchedGDP Year Float
    | FetchedElectricity Year Float

type alias Year = Int

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    FetchedName n -> ({model| name= Just n}, Cmd.none)
    FetchedGDP y gdp -> ({model| gdp= Just (y, gdp)}, Cmd.none)
    FetchedElectricity y percent -> ({model| electricity= Just (y, percent)}, Cmd.none)
    FailedToFetch e -> ({model | lastError = Just (Debug.log "error" e)}, Cmd.none)
-- VIEW

unknown : Maybe String -> String
unknown = Maybe.withDefault "???"

view: Model -> Html Msg
view model = div [] [
    h1 [] [text (model.name |> unknown)],
    h2 [] [text ("GDP: " ++ (model.gdp |> Maybe.map (\(y,v) -> fromFloat v ++ " (" ++ fromInt y ++ ")")|> unknown))],
    h2 [] [text ("Rural Population: " ++ (model.electricity |> Maybe.map (\(y,v) -> fromFloat v ++ "% (" ++ fromInt y ++ ")")|> unknown))]
 ]

-- ACTIONS
fetchCountryName: CountryCode -> Cmd Msg
fetchCountryName = getCountryName <|
    \r -> case r of
        Ok name -> FetchedName name
        Err e -> FailedToFetch e

fetchCountryGDP: CountryCode -> Cmd Msg
fetchCountryGDP = getGDP Latest <|
    \r -> case r of
        Ok [(year, gdp)] -> FetchedGDP year gdp
        Err e -> FailedToFetch e
        _ -> FailedToFetch <| Http.BadBody "Got to many matches..."

fetchElectricity: CountryCode -> Cmd Msg
fetchElectricity = getRuralPop Latest <|
    \r -> case r of
        Ok [(year, gdp)] -> FetchedElectricity year gdp
        Err e -> FailedToFetch e
        _ -> FailedToFetch <| Http.BadBody "Got to many matches..."