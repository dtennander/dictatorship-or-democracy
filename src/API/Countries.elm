module API.Countries exposing (
    Time(..),
    getCountryName,
    getCountryGDP,
    getCountryRuralPop
 , getSchoolEnrolment, getCO2PerCapita, getPoliticalData, FreedomStatus(..))

import Http
import Json.Decode as Decode exposing (Decoder, field, float, index, int, list, maybe, string)
import String exposing (toInt)
import Country exposing (Country)

{-| Command that queries the World Bank for the full name of a Country.
    Takes a Message constructor that wll construct the sent message.
-}
getCountryName : (Result Http.Error String -> m) -> Country -> Cmd m
getCountryName toMsg cc = Http.get {
        url = "http://api.worldbank.org/v2/country/" ++ (Country.asIso2 cc) ++ "?format=json",
        expect = Http.expectJson toMsg
            (index 1 <| index 0 <| field "name" string)
    }


{-| Represents a time span for which to get the resulting data -}
type Time = Latest | Exact Year | Range Year Year

{-| Returns a list of the Country's GDP for each year in the given Time span. -}
getCountryGDP : Time -> (Result Http.Error (List (Year, Float)) -> m) -> Country -> Cmd m
getCountryGDP = getIndicator "NY.GDP.MKTP.CD"
{-| Returns a list of the Country's Ruraö population for each year in the given Time span. -}
getCountryRuralPop : Time -> (Result Http.Error (List (Year, Float)) -> m) -> Country -> Cmd m
getCountryRuralPop = getIndicator "SP.RUR.TOTL.ZS"

getSchoolEnrolment : Time -> (Result Http.Error (List (Year, Float)) -> m) -> Country -> Cmd m
getSchoolEnrolment = getIndicator "SE.PRM.ENRR"
getCO2PerCapita : Time -> (Result Http.Error (List (Year, Float)) -> m) -> Country -> Cmd m
getCO2PerCapita = getIndicator "EN.ATM.CO2E.PC"


type alias Indicator = String
type alias Year = Int

getIndicator : Indicator -> Time -> (Result Http.Error (List (Year, Float)) -> m) -> Country -> Cmd m
getIndicator indicator t toMsg cc =
    let
        decodeYear = field "date" string |> Decode.map (toInt >> Maybe.withDefault 0)
        gatherValues = List.foldr (\v l ->
            case v of
                (y, Just val) -> (y, val) :: l
                (_, Nothing) -> l
            ) []
        filter = case t of
            Latest -> List.sortBy (\(year,_) -> year) >> List.reverse >> gatherValues >> List.take 1
            Exact year -> List.filter (\(y,_) -> y == year) >> gatherValues
            Range from to -> List.filter (\(y,_) -> from <= y && y <= to) >> gatherValues
    in Http.get {
         url = "http://api.worldbank.org/v2/country/" ++ (Country.asIso2 cc) ++ "/indicator/"++ indicator ++"?format=json",
         expect = Http.expectJson (Result.map filter >> toMsg)
             (index 1 <| list <| Decode.map2 (\a b -> (a, b)) decodeYear (maybe <| field "value" float))
     }

type FreedomStatus =
    Free
    | PartlyFree
    | NotFree

getPoliticalData : (Result Http.Error FreedomStatus -> msg) -> Country -> Cmd msg
getPoliticalData toMsg cc =
    let
        toStatus i = case i // 10000 of
            1 -> Ok NotFree
            2 -> Ok PartlyFree
            3 -> Ok Free
            _ -> Err <| Http.BadBody "Could not parse freedom status"
    in Http.get {
        url = ("https://tcdata360-backend.worldbank.org/api/v1/data?indicators=40987&countries=" ++ (Country.asIso3 cc)),
        expect = Http.expectJson (Result.andThen toStatus >> toMsg)
            (field "data"
            <| index 0
            <| field "indicators"
            <| index 0
            <| field "values"
            <| field "2018" int)
     }
