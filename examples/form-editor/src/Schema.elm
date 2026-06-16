module Schema exposing (GroupParams, groupParamsDecoder, fieldDecoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type alias GroupParams =
    { name : String
    , label : Dict String String
    , inline : Bool
    }


groupParamsDecoder : Decoder GroupParams
groupParamsDecoder =
    Decode.succeed
        { name = ""
        , label = Dict.empty
        , inline = False
        }


fieldDecoder : Decoder ()
fieldDecoder =
    Decode.succeed ()
