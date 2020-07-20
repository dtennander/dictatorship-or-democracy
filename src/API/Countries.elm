module API.Countries exposing (parseCountryCode, getCountryName, getGDP, CountryCode, Time(..), getRuralPop)

import Http
import Json.Decode as Decode exposing (Decoder, field, float, index, list, string)
import String exposing (toInt)

type CountryCode = CountryCode Char Char

parseCountryCode : String -> Maybe CountryCode
parseCountryCode = String.toList >> \l -> case l of
    [a,b] -> Just <| CountryCode a b
    _ -> Nothing

iso2Code : CountryCode -> String
iso2Code (CountryCode a b) = String.fromList [a,b]

type alias Getter r m = Time -> (Result Http.Error r -> m) -> CountryCode -> Cmd m

getCountryName : (Result Http.Error String -> m) -> CountryCode -> Cmd m
getCountryName toMsg cc = Http.get {
        url = "http://api.worldbank.org/v2/country/" ++ (iso2Code cc) ++ "?format=json",
        expect = Http.expectJson toMsg
            (index 1 <| index 0 <| field "name" string)
    }

type alias Indicator = String

type alias Year = Int
type Time = Latest | Exact Year | Range Year Year

getIndicator : Indicator -> Getter (List (Year, Float)) m
getIndicator indicator t toMsg cc =
    let
        decodeYear = field "date" string |> Decode.map (toInt >> Maybe.withDefault 0)
        filter = case t of
            Latest -> List.sortBy (\(year,_) -> year) >> List.reverse >> List.take 1
            Exact year -> List.filter (\(y,_) -> y == year)
            Range from to -> List.filter (\(y,_) -> from <= y && y <= to)
    in Http.get {
         url = "http://api.worldbank.org/v2/country/" ++ (iso2Code cc) ++ "/indicator/"++ indicator ++"?format=json",
         expect = Http.expectJson (Result.map filter >> toMsg)
             (index 1 <| list <| Decode.map2 (\a b -> (a, b)) decodeYear (field "value" float))
     }


getGDP = getIndicator "NY.GDP.MKTP.CD"
getMiddleHeadCount = getIndicator "1.0.HCount.Mid10to50"
getRuralPop = getIndicator "SP.RUR.TOTL.ZS"