module API.Countries exposing (
    Country,
    parseCountryCode,
    iso2Code,

    Time(..),
    getCountryName,
    getCountryGDP,
    getCountryRuralPop
 , getSchoolEnrolment, getCO2PerCapita)

import Http
import Json.Decode as Decode exposing (Decoder, field, float, index, list, maybe, string)
import String exposing (toInt)

{-| Represents one country on Earth.
Is created by using parseCountryCode.
-}
type Country = CountryCode Char Char

{-|Parses a string as an ISO2 country code.
Returns a Country if it is a valid code and Nothing otherwise.
-}
parseCountryCode : String -> Maybe Country
parseCountryCode = String.toList >> \l -> case l of
    [a,b] -> Just <| CountryCode a b
    _ -> Nothing

{-| Generates the ISO 2 Country code for a given country.
-}
iso2Code : Country -> String
iso2Code (CountryCode a b) = String.fromList [a,b]

{-| Command that queries the World Bank for the full name of a Country.
    Takes a Message constructor that wll construct the sent message.
-}
getCountryName : (Result Http.Error String -> m) -> Country -> Cmd m
getCountryName toMsg cc = Http.get {
        url = "http://api.worldbank.org/v2/country/" ++ (iso2Code cc) ++ "?format=json",
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
         url = "http://api.worldbank.org/v2/country/" ++ (iso2Code cc) ++ "/indicator/"++ indicator ++"?format=json",
         expect = Http.expectJson (Result.map filter >> toMsg)
             (index 1 <| list <| Decode.map2 (\a b -> (a, b)) decodeYear (maybe <| field "value" float))
     }