module Page.Question exposing (view, Model, init, update, Msg)

import Country exposing (Country)
import Debug
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (field, index)
import Random
import String exposing (fromFloat, fromInt, toInt)

---- MODEL ----
type Model = HasCountry Data | AwaitingCountry | Result String Bool

type alias Data = {
        country: Country,
        name: Maybe String,
        government: Maybe Answer,
        ruralPopulation: Maybe (Year, Float),
        schoolEnrolment: Maybe (Year, Float),
        co2PerCapita: Maybe (Year, Float),
        lastFetchError: Maybe Http.Error
    }

emptyDataset c =  {
   country = c,
   name = Nothing,
   lastFetchError = Nothing,
   ruralPopulation = Nothing,
   schoolEnrolment = Nothing,
   co2PerCapita = Nothing,
   government = Nothing}

init: (Model, Cmd Msg)
init = (AwaitingCountry, getRandomCountry)

---- UPDATE ----
type Msg =
    PickedCountry Country
    | FetchedDatapoint (Result Http.Error DataPoint)
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
    (AwaitingCountry, PickedCountry c) ->
        (HasCountry <| emptyDataset c, fetchAll c)
    (HasCountry d, FetchedDatapoint (Ok dp)) ->
        (HasCountry (updateDateSet dp d), Cmd.none)
    (HasCountry d, FetchedDatapoint (Err e)) ->
        (HasCountry {d | lastFetchError = Just (Debug.log "error" e)}, Cmd.none)
    (HasCountry d, GotAnswer answer) ->
        case (d.government, d.name) of
            (Just g, Just n) -> (Result n (g == answer), Cmd.none)
            _ -> (model, Cmd.none)
    (_, _) -> (model, Cmd.none)

updateDateSet : DataPoint -> Data -> Data
updateDateSet dp d = case dp of
    Name n -> {d | name = Just n}
    RuralPopulation v -> {d | ruralPopulation = Just v}
    SchoolEnrolment v -> {d | schoolEnrolment = Just v}
    CO2PerCapita v -> {d | co2PerCapita = Just v}
    FreedomStatus NotFree -> {d | government = Just Dictatorship}
    FreedomStatus _ -> {d | government = Just Democracy}


---- VIEW ----

view: Model -> Html Msg
view m = case m of
    HasCountry d -> viewValid d
    AwaitingCountry -> h1 [] [text "Picking Country..."]
    Result c True -> div [] [
            h1 [] [text "Congratulations, you where correct!!"],
            h3 [] [text <| "The country was " ++ c ++ "!"]
        ]
    Result c False -> div [] [
            h1 [] [text "That was unfortunately incorrect..."],
            h3 [] [text <| "The country was " ++ c ++ "!"]
        ]


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

getRandomCountry = Random.generate PickedCountry randomCountry

randomCountry : Random.Generator Country
randomCountry =
    Random.map2 (\a b -> String.fromList [a, b]) randomChar randomChar
        |> Random.andThen (\s -> case Country.parse s of
            Just c -> Random.constant c
            Nothing -> randomCountry)

randomChar = Random.int (Char.toCode 'A') (Char.toCode 'Z') |> Random.map Char.fromCode

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