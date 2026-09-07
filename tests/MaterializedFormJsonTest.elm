module MaterializedFormJsonTest exposing (suite)

{- Tests for the materialized (filled-in) form JSON codec.

A materialized form is a FormToolkit.Field tree carrying user values. Its JSON
is a nested object keyed by input names with scalar values.

The current codec is a pair of independent functions:

  - encode: Parse.json   : Parser id Json.Encode.Value
  - decode: Field.updateValuesFromJson : Encode.Value -> Field id -> Result (Error id) (Field id)

Values are serialized in typed form (numbers as numbers, dates/months/times as
their canonical strings) and parsed back according to the input type, so typed
values (int, float, bool, date, month, time) survive a round-trip.

Note on structure: updateValuesFromJson resolves JSON keys against Field.name
attributes walking the tree *below* the root, so every input under test is
wrapped in an unnamed root group (as all real forms are; see the doc example
on Field.updateValuesFromJson).

Note on select and radio: Parse.json encodes the chosen option value, while
updateValuesFromJson resolves incoming strings as option *indices* (the same
path as user input events, whose DOM option values are indices). The two are
therefore not inverses for those input types; select and radio are covered by
encode-shape tests below instead of generic round-trips.
-}

import Expect
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value exposing (Value)
import Json.Decode as Decode
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Materialized form JSON codec"
        [ describe "round-trips values through Parse.json and updateValuesFromJson"
            [ roundTrips "text" textInput (Value.string "hello")
            , roundTrips "textarea" textareaInput (Value.string "line1\nline2")
            , roundTrips "email" emailInput (Value.string "alice@example.com")
            , roundTrips "url" urlInput (Value.string "https://example.com")
            , roundTrips "password" passwordInput (Value.string "s3cret")
            , roundTrips "int" intInput (Value.int 42)
            , roundTrips "float" floatInput (Value.float 3.14)
            , roundTrips "date" dateInput (Value.date (Time.millisToPosix 0))
            , roundTrips "month" monthInput (Value.month (Time.millisToPosix 0))
            , roundTrips "datetime" datetimeInput (Value.time (Time.millisToPosix 0))
            , roundTrips "checkbox" checkboxInput (Value.bool True)
            ]
        , describe "select and radio encode their chosen option value"
            [ test "select" <|
                \_ ->
                    selectInput
                        |> Parse.parse Parse.json
                        |> Result.mapError (always "Parse.json failed")
                        |> Result.andThen decodeValueField
                        |> Expect.equal (Ok "b")
            , test "radio" <|
                \_ ->
                    radioInput
                        |> Parse.parse Parse.json
                        |> Result.mapError (always "Parse.json failed")
                        |> Result.andThen decodeValueField
                        |> Expect.equal (Ok "a")
            ]
        , describe "structure"
            [ test "Parse.json produces name-keyed nested JSON" <|
                \_ ->
                    nestedForm
                        |> Parse.parse Parse.json
                        |> Result.mapError (always "Parse.json failed")
                        |> Result.andThen
                            (Decode.decodeValue (Decode.at [ "user", "name" ] Decode.string)
                                >> Result.mapError Decode.errorToString
                            )
                        |> Expect.equal (Ok "Alice")
            , test "nested group values round-trip" <|
                \_ ->
                    nestedForm
                        |> Parse.parse Parse.json
                        |> Result.andThen (\json -> Field.updateValuesFromJson json blankNestedForm)
                        |> Result.andThen
                            (Parse.parse
                                (Parse.map2 Tuple.pair
                                    (Parse.field "name-field" Parse.string)
                                    (Parse.field "email-field" Parse.string)
                                )
                            )
                        |> Expect.equal (Ok ( "Alice", "alice@example.com" ))
            ]
        ]


decodeValueField : Decode.Value -> Result String String
decodeValueField json =
    Decode.decodeValue (Decode.field "value" Decode.string) json
        |> Result.mapError Decode.errorToString


roundTrips : String -> Field.Field String -> Value -> Test
roundTrips label field_ expected =
    test label <|
        \_ ->
            Field.group [] [ field_ ]
                |> Parse.parse Parse.json
                |> Result.andThen (\json -> Field.updateValuesFromJson json (Field.group [] [ field_ ]))
                |> Result.andThen (Parse.parse (Parse.field "id" Parse.value))
                |> Expect.equal (Ok expected)



-- Single-input fixtures (each named "value", identified "id")


textInput : Field.Field String
textInput =
    Field.text [ Field.name "value", Field.identifier "id", Field.value (Value.string "hello") ]


textareaInput : Field.Field String
textareaInput =
    Field.textarea [ Field.name "value", Field.identifier "id", Field.value (Value.string "line1\nline2") ]


emailInput : Field.Field String
emailInput =
    Field.email [ Field.name "value", Field.identifier "id", Field.value (Value.string "alice@example.com") ]


urlInput : Field.Field String
urlInput =
    Field.url [ Field.name "value", Field.identifier "id", Field.value (Value.string "https://example.com") ]


passwordInput : Field.Field String
passwordInput =
    Field.password [ Field.name "value", Field.identifier "id", Field.value (Value.string "s3cret") ]


intInput : Field.Field String
intInput =
    Field.int [ Field.name "value", Field.identifier "id", Field.value (Value.int 42) ]


floatInput : Field.Field String
floatInput =
    Field.float [ Field.name "value", Field.identifier "id", Field.value (Value.float 3.14) ]


dateInput : Field.Field String
dateInput =
    Field.date [ Field.name "value", Field.identifier "id", Field.value (Value.date (Time.millisToPosix 0)) ]


monthInput : Field.Field String
monthInput =
    Field.month [ Field.name "value", Field.identifier "id", Field.value (Value.month (Time.millisToPosix 0)) ]


datetimeInput : Field.Field String
datetimeInput =
    Field.datetime [ Field.name "value", Field.identifier "id", Field.value (Value.time (Time.millisToPosix 0)) ]


selectInput : Field.Field String
selectInput =
    Field.select
        [ Field.name "value"
        , Field.identifier "id"
        , Field.stringOptions [ "a", "b", "c" ]
        , Field.value (Value.string "b")
        ]


radioInput : Field.Field String
radioInput =
    Field.radio
        [ Field.name "value"
        , Field.identifier "id"
        , Field.stringOptions [ "a", "b" ]
        , Field.value (Value.string "a")
        ]


checkboxInput : Field.Field String
checkboxInput =
    Field.checkbox [ Field.name "value", Field.identifier "id", Field.value (Value.bool True) ]



-- Nested fixtures


nestedForm : Field.Field String
nestedForm =
    Field.group []
        [ Field.group [ Field.name "user" ]
            [ Field.text [ Field.name "name", Field.identifier "name-field", Field.value (Value.string "Alice") ]
            , Field.text [ Field.name "email", Field.identifier "email-field", Field.value (Value.string "alice@example.com") ]
            ]
        ]


blankNestedForm : Field.Field String
blankNestedForm =
    Field.group []
        [ Field.group [ Field.name "user" ]
            [ Field.text [ Field.name "name", Field.identifier "name-field" ]
            , Field.text [ Field.name "email", Field.identifier "email-field" ]
            ]
        ]
