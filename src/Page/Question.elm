module Page.Question exposing (view, Model, init, update, Msg)

import Country exposing (Country)
import Debug
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (field, index)
import String exposing (fromFloat, fromInt, toInt)

---- MODEL ----
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

---- UPDATE ----
type Msg =
    FetchedDatapoint (Result Http.Error DataPoint)
    | GotAnswer Answer

type Answer = Dictatorship | Democracy

type DataPoint =
      Name String
    | RuralPopulation (Year, Float)
    | SchoolEnrolment (Year, Float)
    | CO2PerCapita (Year, Float)
    | FreedomStatus FreedomStatus

type alias Year = Int

type FreedomStatus =
    Free
    | PartlyFree
    | NotFree

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = case (model, msg) of
    (Valid c d, FetchedDatapoint (Ok dp)) ->
        (Valid c (updateData dp d), Cmd.none)
    (Valid c d, FetchedDatapoint (Err e)) ->
        (Valid c {d | lastFetchError = Just (Debug.log "error" e)}, Cmd.none)
    (Valid _ d, GotAnswer answer) ->
        case d.government of
            Just g -> (Result (g == answer), Cmd.none)
            Nothing -> (model, Cmd.none)
    (_, _) -> (model, Cmd.none)

updateData : DataPoint -> Data -> Data
updateData dp d = case dp of
    Name n -> {d | name = Just n}
    RuralPopulation v -> {d | ruralPopulation = Just v}
    SchoolEnrolment v -> {d | schoolEnrolment = Just v}
    CO2PerCapita v -> {d | co2PerCapita = Just v}
    FreedomStatus NotFree -> {d | government = Just Dictatorship}
    FreedomStatus _ -> {d | government = Just Democracy}


---- VIEW ----

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

---- ACTIONS ----

fetchAll c = Cmd.batch
    [ fetchName c
    , getIndicator "SP.RUR.TOTL.ZS" (Result.map RuralPopulation >> FetchedDatapoint) c
    , getIndicator "SE.PRM.ENRR" (Result.map SchoolEnrolment >> FetchedDatapoint) c
    , getIndicator "EN.ATM.CO2E.PC" (Result.map CO2PerCapita >> FetchedDatapoint) c
    , getPoliticalData c
    ]

fetchName c = Http.get {
       url = "http://api.worldbank.org/v2/country/" ++ (Country.asIso2 c) ++ "?format=json",
       expect = Http.expectJson (Result.map Name >> FetchedDatapoint) nameDecoder
   }

getIndicator indicator toMsg cc = Http.get {
         url = "http://api.worldbank.org/v2/country/" ++ (Country.asIso2 cc) ++ "/indicator/"++ indicator ++"?format=json",
         expect = Http.expectJson toMsg indicatorDecoder
     }

getPoliticalData cc = Http.get {
        url = ("https://tcdata360-backend.worldbank.org/api/v1/data?indicators=40987&countries=" ++ (Country.asIso3 cc)),
        expect = Http.expectJson (Result.map FreedomStatus >> FetchedDatapoint) politicalFreedomDecoder
     }

---- DECODERS ----
nameDecoder = index 1 <| index 0 <| field "name" Decode.string
indicatorDecoder = index 1 decodeLatestYear
decodeLatestYear =
    let
        takeFirst = List.sortBy Tuple.first
            >> List.reverse
            >> List.head
            >> Maybe.map Decode.succeed
            >> Maybe.withDefault (Decode.fail "No data found for any year...")
    in Decode.list decodeDatapoint |> Decode.map (List.filterMap identity) |> Decode.andThen takeFirst

decodeDatapoint = Decode.maybe <| Decode.map2 Tuple.pair decodeYear (field "value" Decode.float)
decodeYear = field "date" Decode.string |> Decode.map (toInt >> Maybe.withDefault 0)

politicalFreedomDecoder = field "data"
                          <| index 0
                          <| field "indicators"
                          <| index 0
                          <| field "values"
                          <| field "2018" statusDecoder

statusDecoder =
    let asStatus i = case i // 10000 of
         1 -> Decode.succeed NotFree
         2 -> Decode.succeed PartlyFree
         3 -> Decode.succeed Free
         _ -> Decode.fail    "Could not parse freedom status"
    in Decode.int |> Decode.andThen asStatus