module ManicPixie.Rest exposing (fetchNames)

import Http exposing (..)
import Json.Decode as D exposing (Decoder)
import Types exposing (..)


type Msg
    = GotItems (Result Http.Error (List String))


fetchNames : Cmd Msg
fetchNames =
    Http.get
        { url = "http://names.drycodes.com/10?nameOptions=girl_names"
        , expect = Http.expectJson GotItems (D.list (D.field "name" D.string))
        }



-- DECODER


nameDecoder : Decoder String
nameDecoder =
    Decode.index 0 Decode.string
