module Rest exposing (Girl,  Msg(..), getRandomNames, getRandomQuote, nameDecoder)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, index, list, map4, string)

type Msg
    = MorePlease
    | RollFeatures
    | RollAgeHobbies 
    | SetFeatures ( Int, Int )
    | SetAge ( Int, Int )
    | GotName (Result Http.Error Girl)
    | GotQuote (Result Http.Error String)
    | ChainMsgs (List Msg)


getRandomQuote : Cmd Msg
getRandomQuote =
    Http.get
        { url = "https://favqs.com/api/qotd"

        -- { url = "http://localhost:8001/data/names.json"
        , expect = Http.expectJson GotQuote quoteDecoder
        }


quoteDecoder : Decoder String
quoteDecoder =
    (field "quote" (field "body" string))



-- getRandomNames Http get command


getRandomNames : Cmd Msg
getRandomNames =
    Http.get
        { url = "https://randomuser.me/api/?gender=female"

        -- { url = "http://localhost:8001/data/names.json"
        , expect = Http.expectJson GotName nameDecoder
        }


type alias Girl =
    { firstName : String
    , lastName : String
    , city : String
    , state : String
    }


nameDecoder : Decoder Girl
nameDecoder =
    map4 Girl
        (field "results" (index 0 (field "name" (field "first" string))))
        (field "results" (index 0 (field "name" (field "last" string))))
        (field "results" (index 0 (field "location" (field "city" string))))
        (field "results" (index 0 (field "location" (field "state" string))))