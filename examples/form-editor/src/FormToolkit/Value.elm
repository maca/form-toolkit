module FormToolkit.Value exposing (Value, blank, boolean, string, toString, toBool, encode)

import Json.Encode as Encode

type Value
    = Blank
    | String String
    | Boolean Bool

blank : Value
blank =
    Blank

string : String -> Value
string =
    String

boolean : Bool -> Value
boolean =
    Boolean

toString : Value -> String
toString val =
    case val of
        Blank ->
            ""
        String s ->
            s
        Boolean b ->
            if b then "true" else "false"

toBool : Value -> Bool
toBool val =
    case val of
        Boolean b ->
            b
        _ ->
            False


encode : Value -> Encode.Value
encode val =
    case val of
        Blank ->
            Encode.null

        String s ->
            Encode.string s

        Boolean b ->
            Encode.bool b
