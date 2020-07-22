module Page.Question exposing (view, Model, init, update, Msg)

import API.Codes exposing (Country, parseCountryCode)
import API.Countries exposing (FreedomStatus(..), Time(..), getCO2PerCapita, getCountryName, getCountryRuralPop, getPoliticalData, getSchoolEnrolment)
import Debug
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import String exposing (fromFloat, fromInt)

-- MODEL
type Model = Valid Country Data | Invalid | Result Bool

type alias Data = {
        name: Maybe String,
        ruralPopulation: Maybe (Year, Float),
        schoolEnrolment: Maybe (Year, Float),
        co2PerCapita: Maybe (Year, Float),
        government: Maybe Answer,
        lastFetchError: Maybe Http.Error
    }

init: String -> (Model, Cmd Msg)
init cc =
    let baseModel = {
            name = Nothing,
            lastFetchError = Nothing,
            ruralPopulation = Nothing,
            schoolEnrolment = Nothing,
            co2PerCapita = Nothing,
            government = Nothing
            }
    in case parseCountryCode cc of
        Just c -> (Valid c baseModel, Cmd.batch [
                fetchName c,
                fetchRuralPopulation c,
                fetchSchoolEnrolment c,
                fetchCo2 c,
                fetchFreedomStatus c
            ])
        Nothing -> (Invalid, Cmd.none)


-- UPDATE
type Msg =
      FailedToFetch Http.Error
    | FetchedDatapoint Datapoint
    | GotAnswer Answer

type Datapoint =
      Name String
    | RuralPopulation Year Float
    | SchoolEnrolment Year Float
    | CO2PerCapita Year Float
    | FreedomStatus FreedomStatus

type alias Year = Int

type Answer = Dictatorship | Democracy

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = case (model, msg) of
    (Valid c d, FetchedDatapoint dp) ->
        (Valid c (updateData dp d), Cmd.none)
    (Valid c m, FailedToFetch e) ->
        (Valid c {m | lastFetchError = Just (Debug.log "error" e)}, Cmd.none)
    (Valid _ d, GotAnswer answer) ->
        case d.government of
            Just g -> (Result (g == answer), Cmd.none)
            Nothing -> (model, Cmd.none)
    (_, _) -> (model, Cmd.none)

updateData : Datapoint -> Data -> Data
updateData dp d = case dp of
    Name n -> {d | name = Just n}
    RuralPopulation y v -> {d | ruralPopulation = Just (y,v)}
    SchoolEnrolment y v -> {d | schoolEnrolment = Just (y,v)}
    CO2PerCapita y v -> {d | co2PerCapita = Just (y,v)}
    FreedomStatus NotFree -> {d | government = Just Dictatorship}
    FreedomStatus _ -> {d | government = Just Democracy}

-- VIEW

view: Model -> Html Msg
view m = case m of
    Valid _ d -> viewValid d
    Invalid -> h1 [] [text "No country could be parsed from the given Country Code"]
    Result True -> h1 [] [text "Congratulations, you where correct!"]
    Result False -> h1 [] [text "That was unfortunately incorrect..."]

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
        stat "CO2 releases per Capita [Ton]" d.co2PerCapita,
        div [] [
            button [onClick <| GotAnswer Dictatorship] [text "Dictatorship!"],
            button [onClick <| GotAnswer Democracy] [text "Democracy!"]
        ]
    ]

-- ACTIONS
fetchName = getCountryName <|
    \r -> case r of
        Ok name -> FetchedDatapoint (Name name)
        Err e -> FailedToFetch e

fetchRuralPopulation = fetchWith getCountryRuralPop (asSingleDatapoint RuralPopulation)
fetchSchoolEnrolment = fetchWith getSchoolEnrolment (asSingleDatapoint SchoolEnrolment)
fetchCo2 = fetchWith getCO2PerCapita (asSingleDatapoint CO2PerCapita)
fetchFreedomStatus = fetchWith (always getPoliticalData) (FreedomStatus >> Just)

asSingleDatapoint : (a -> b -> c) -> List (a, b) -> Maybe c
asSingleDatapoint f = List.head >> Maybe.map (\(a,b) -> f a b)

type alias Fetcher a = Time -> (Result Http.Error a -> Msg) -> Country -> Cmd Msg

fetchWith: Fetcher a-> (a -> Maybe Datapoint) -> Country -> Cmd Msg
fetchWith fetcher toDataPoint = fetcher Latest <|
    \r -> case r of
       Ok a -> case toDataPoint a of
          Just dp -> FetchedDatapoint dp
          Nothing -> FailedToFetch (Http.BadBody "Could not transform result to Datapoint")
       Err e -> FailedToFetch e