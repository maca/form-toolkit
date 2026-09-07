module MaterializedFormJsonTest exposing (suite)

{- Tests for the materialized (filled-in) form JSON codec.

A materialized form is a FormToolkit.Field tree carrying user values. Its JSON
is a nested object keyed by input names with scalar values:

  - Field.encodeValues : Field id -> Encode.Value
  - Field.updateValuesFromJson : Encode.Value -> Field id -> Result (Error id) (Field id)

Values are serialized by their string form and parsed back according to the
input type, so typed values (int, float, bool, date, month, time) survive a
round-trip.
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
        [ describe "round-trips every input variant"
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
            , roundTrips "select" selectInput (Value.string "b")
            , roundTrips "radio" radioInput (Value.string "a")
            , roundTrips "checkbox" checkboxInput (Value.bool True)
            ]
        , describe "structure"
            [ test "encodeValues produces name-keyed nested JSON" <|
                \_ ->
                    nestedForm
                        |> Field.encodeValues
                        |> Decode.decodeValue (Decode.at [ "user", "name" ] Decode.string)
                        |> Expect.equal (Ok "Alice")
            , test "nested group values round-trip" <|
                \_ ->
                    nestedForm
                        |> Field.encodeValues
                        |> (\json -> Field.updateValuesFromJson json blankNestedForm)
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


roundTrips : String -> Field.Field String -> Value -> Test
roundTrips label field_ expected =
    test label <|
        \_ ->
            field_
                |> Field.encodeValues
                |> (\json -> Field.updateValuesFromJson json field_)
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
