module Page.Question exposing (view, Model, init, update, Msg)

import API.Countries exposing (Country, Time(..), getCO2PerCapita, getCountryGDP, getCountryName, getCountryRuralPop, getSchoolEnrolment, parseCountryCode)
import Debug
import Html exposing (..)
import Http
import String exposing (fromFloat, fromInt)

-- MODEL
type Model = Valid Country Data | Invalid

type alias Data = {
        name: Maybe String,
        ruralPopulation: Maybe (Year, Float),
        schoolEnrolment: Maybe (Year, Float),
        co2PerCapita: Maybe (Year, Float),
        lastFetchError: Maybe Http.Error
    }

init: String -> (Model, Cmd Msg)
init cc =
    let baseModel = {
            name = Nothing,
            lastFetchError = Nothing,
            ruralPopulation = Nothing,
            schoolEnrolment = Nothing,
            co2PerCapita = Nothing
            }
    in case parseCountryCode cc of
        Just c -> (Valid c baseModel, Cmd.batch [
                fetchName c,
                fetchRuralPopulation c,
                fetchSchoolEnrolment c,
                fetchCo2 c
            ])
        Nothing -> (Invalid, Cmd.none)


-- UPDATE
type Msg =
      FailedToFetch Http.Error
    | FetchedDatapoint Datapoint

type Datapoint =
      Name String
    | RuralPopulation Year Float
    | SchoolEnrolment Year Float
    | CO2PerCapita Year Float

type alias Year = Int

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = case (model, msg) of
    (Valid c d, FetchedDatapoint dp) ->
        (Valid c (updateData dp d), Cmd.none)
    (Valid c m, FailedToFetch e) ->
        (Valid c {m | lastFetchError = Just (Debug.log "error" e)}, Cmd.none)
    (Invalid, _) -> (model, Cmd.none)

updateData : Datapoint -> Data -> Data
updateData dp d = case dp of
    Name n -> {d | name = Just n}
    RuralPopulation y v -> {d | ruralPopulation = Just (y,v)}
    SchoolEnrolment y v -> {d | schoolEnrolment = Just (y,v)}
    CO2PerCapita y v -> {d | co2PerCapita = Just (y,v)}

-- VIEW

view: Model -> Html Msg
view m = case m of
    Valid _ d -> viewValid d
    Invalid -> h1 [] [text "No country could be parsed from the given Country Code"]

viewValid: Data -> Html Msg
viewValid d =
    let
        toString (y, v) = fromFloat v ++ " (" ++ fromInt y ++ ")"
        stat txt value = h2 [] [
                text (txt ++ ": " ++ (value |> Maybe.map toString |> Maybe.withDefault "???"))
            ]
    in div [] [
        h1 [] [text "Is this a Dictatorship or Democracy?"],
        stat "Rural Population [%]" d.ruralPopulation,
        stat "School Enrolment [%]" d.schoolEnrolment,
        stat "CO2 releases per Capita [Ton]" d.co2PerCapita
    ]

-- ACTIONS
fetchName = getCountryName <|
    \r -> case r of
        Ok name -> FetchedDatapoint (Name name)
        Err e -> FailedToFetch e
fetchRuralPopulation = fetchWith getCountryRuralPop RuralPopulation
fetchSchoolEnrolment = fetchWith getSchoolEnrolment SchoolEnrolment
fetchCo2 = fetchWith getCO2PerCapita CO2PerCapita

type alias Fetcher = Time -> (Result Http.Error (List (Year, Float)) -> Msg) -> Country -> Cmd Msg

fetchWith: Fetcher -> (Year -> Float -> Datapoint) -> Country -> Cmd Msg
fetchWith fetcher toDataPoint = fetcher Latest <|
    \r -> case r of
       Ok [(year, gdp)] -> FetchedDatapoint (toDataPoint year gdp)
       Err e -> FailedToFetch e
       _ -> FailedToFetch <| Http.BadBody "Got to many matches..."