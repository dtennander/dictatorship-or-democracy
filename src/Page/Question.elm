module Page.Question exposing (view, Model, init, update, Msg)

import API.Countries exposing (FreedomStatus(..), Time(..), getCO2PerCapita, getCountryName, getCountryRuralPop, getPoliticalData, getSchoolEnrolment)
import Country exposing (Country)
import Debug
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import String exposing (fromFloat, fromInt)

-- MODEL
type Model = Valid Country Data | Invalid | Result Bool

type alias Data = {
        name: Maybe String,
        government: Maybe Answer,
        ruralPopulation: Maybe (Year, Float),
        schoolEnrolment: Maybe (Year, Float),
        co2PerCapita: Maybe (Year, Float),
        lastFetchError: Maybe Http.Error
    }

init: String -> (Model, Cmd Msg)
init cc = case Country.parse cc of
        Nothing -> (Invalid, Cmd.none)
        Just c -> (
                Valid c {
                    name = Nothing,
                    lastFetchError = Nothing,
                    ruralPopulation = Nothing,
                    schoolEnrolment = Nothing,
                    co2PerCapita = Nothing,
                    government = Nothing},
                 fetchAll c)


-- UPDATE
type Msg =
      FailedToFetch Http.Error
    | FetchedDatapoint Datapoint
    | GotAnswer Answer

type Answer = Dictatorship | Democracy

type alias Year = Int

type Datapoint =
      Name String
    | RuralPopulation Year Float
    | SchoolEnrolment Year Float
    | CO2PerCapita Year Float
    | FreedomStatus FreedomStatus

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = case (model, msg) of
    (Valid c d, FetchedDatapoint dp) ->
        (Valid c (updateData dp d), Cmd.none)
    (Valid c d, FailedToFetch e) ->
        (Valid c {d | lastFetchError = Just (Debug.log "error" e)}, Cmd.none)
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
viewValid d = div [] [
        h1 [] [text "Country Details"],
        viewStat "Rural Population [%]" d.ruralPopulation,
        viewStat "School Enrolment [%]" d.schoolEnrolment,
        viewStat "CO2 releases per Capita [Ton]" d.co2PerCapita,
        h1 [] [text "Is it a Dictatorship or Democracy?"],
        div [] [
            button [onClick <| GotAnswer Dictatorship] [text "Dictatorship!"],
            button [onClick <| GotAnswer Democracy] [text "Democracy!"]
        ]
    ]

viewStat txt value = h2 [] [text (txt ++ ": " ++ (value |> Maybe.map viewDatapoint |> Maybe.withDefault "???"))]
viewDatapoint (y, v) = fromFloat v ++ " (" ++ fromInt y ++ ")"

-- ACTIONS

fetchAll c = Cmd.batch
    [ fetchName c
    , fetchWith getCountryRuralPop (headMap RuralPopulation) c
    , fetchWith getSchoolEnrolment (headMap SchoolEnrolment) c
    , fetchWith getCO2PerCapita (headMap CO2PerCapita) c
    , fetchWith (always getPoliticalData) (FreedomStatus >> Just) c
    ]

fetchName = getCountryName <|
    \r -> case r of
        Ok name -> FetchedDatapoint (Name name)
        Err e -> FailedToFetch e

headMap : (a -> b -> c) -> List (a, b) -> Maybe c
headMap f = List.head >> Maybe.map (\(a,b) -> f a b)

type alias Fetcher a = Time -> (Result Http.Error a -> Msg) -> Country -> Cmd Msg

fetchWith: Fetcher a-> (a -> Maybe Datapoint) -> Country -> Cmd Msg
fetchWith fetcher toDataPoint = fetcher Latest <|
    \r -> case r of
       Ok a -> case toDataPoint a of
          Just dp -> FetchedDatapoint dp
          Nothing -> FailedToFetch (Http.BadBody "Could not transform result to Datapoint")
       Err e -> FailedToFetch e