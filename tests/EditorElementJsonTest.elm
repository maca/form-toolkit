module EditorElementJsonTest exposing (suite)

{- Tests for the editor (form-definition) JSON codec.

The codec is the round-trip pair Editor.Element.encode / Editor.Element.decode:

  - encode : Element -> Maybe Encode.Value
  - decode : Decoder Element

id, drag and isOpen are runtime concerns and are NOT serialized: decode resets
id to Id.unset and drag to Drag.idle, and defaults isOpen to True so a loaded
form is fully expanded.
-}

import Editor.Drag as Drag
import Editor.Element as Element exposing (Element(..), Field(..))
import Editor.Id as Id
import Expect
import FormToolkit.Value as Value
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Editor.Element JSON codec"
        [ describe "round-trips every variant with all attributes"
            [ test "text field" <| \_ -> roundTrip textField
            , test "checkbox field" <| \_ -> roundTrip checkboxField
            , test "integer field with min/max" <| \_ -> roundTrip integerField
            , test "date field with min/max" <| \_ -> roundTrip dateField
            , test "month field with min/max" <| \_ -> roundTrip monthField
            , test "select field with options" <| \_ -> roundTrip selectField
            , test "radio field with options" <| \_ -> roundTrip radioField
            , test "group with a nested field" <| \_ -> roundTrip groupElement
            , test "repeatable group with a nested field" <| \_ -> roundTrip repeatableElement
            , test "review" <| \_ -> roundTrip reviewElement
            , test "help" <| \_ -> roundTrip helpElement
            ]
        , describe "attribute variants"
            [ test "nullable attributes stay Nothing" <| \_ -> roundTrip minimalTextField
            , test "empty select options round-trip" <| \_ -> roundTrip emptySelectField
            , test "blank range bounds round-trip" <| \_ -> roundTrip blankRangeField
            , test "encode Blank is Nothing" <|
                \_ ->
                    Element.encode (Blank Id.unset)
                        |> Expect.equal Nothing
            ]
        , describe "encode shape"
            [ test "encodes hint from the hint attribute, not help" <|
                \_ ->
                    textField
                        |> encodedField
                            (Decode.map2 Tuple.pair
                                (Decode.field "hint" Decode.string)
                                (Decode.field "help" Decode.string)
                            )
                        |> Expect.equal
                            (Just (Ok ( "Your given name", "As it appears on your passport" )))
            , test "encodes the field type tag" <|
                \_ ->
                    textField
                        |> encodedField (Decode.field "type" Decode.string)
                        |> Expect.equal (Just (Ok "text"))
            , test "encodes select options as value/label pairs" <|
                \_ ->
                    Element.select [ ( "en", "English" ), ( "es", "Spanish" ) ]
                        |> encodedField
                            (Decode.field "options"
                                (Decode.list
                                    (Decode.map2 Tuple.pair
                                        (Decode.field "value" Decode.string)
                                        (Decode.field "label" Decode.string)
                                    )
                                )
                            )
                        |> Expect.equal
                            (Just (Ok [ ( "en", "English" ), ( "es", "Spanish" ) ]))
            ]
        ]


roundTrip : Element -> Expect.Expectation
roundTrip element =
    element
        |> Element.encode
        |> Maybe.map (Decode.decodeValue Element.decode)
        |> Expect.equal (Just (Ok element))


encodedField : Decode.Decoder a -> Element -> Maybe (Result Decode.Error a)
encodedField fieldDecoder element =
    element
        |> Element.encode
        |> Maybe.map (Decode.decodeValue fieldDecoder)



-- Element fixtures


textField : Element
textField =
    FieldElement
        { id = Id.unset
        , field = TextField
        , name = Just "first_name"
        , label = Just "First name"
        , placeholder = Just "e.g. Frank"
        , hint = Just "Your given name"
        , help = Just "As it appears on your passport"
        , isRequired = True
        , drag = Drag.idle
        }


checkboxField : Element
checkboxField =
    FieldElement
        { id = Id.unset
        , field = Checkbox
        , name = Just "subscribe"
        , label = Just "Subscribe to the newsletter"
        , placeholder = Nothing
        , hint = Just "We send at most one email a month"
        , help = Just "You can unsubscribe at any time"
        , isRequired = False
        , drag = Drag.idle
        }


integerField : Element
integerField =
    FieldElement
        { id = Id.unset
        , field = IntegerField { min = Value.int 0, max = Value.int 100 }
        , name = Just "age"
        , label = Just "Age"
        , placeholder = Just "42"
        , hint = Just "Your age in years"
        , help = Just "Must be at least 18"
        , isRequired = True
        , drag = Drag.idle
        }


dateField : Element
dateField =
    FieldElement
        { id = Id.unset
        , field = DateField { min = Value.date (Time.millisToPosix 0), max = Value.date (Time.millisToPosix 0) }
        , name = Just "birth_date"
        , label = Just "Birth date"
        , placeholder = Nothing
        , hint = Just "When were you born?"
        , help = Just "Used to calculate age"
        , isRequired = True
        , drag = Drag.idle
        }


monthField : Element
monthField =
    FieldElement
        { id = Id.unset
        , field = MonthField { min = Value.month (Time.millisToPosix 0), max = Value.month (Time.millisToPosix 0) }
        , name = Just "start_month"
        , label = Just "Start month"
        , placeholder = Nothing
        , hint = Nothing
        , help = Nothing
        , isRequired = False
        , drag = Drag.idle
        }


selectField : Element
selectField =
    FieldElement
        { id = Id.unset
        , field = Select [ ( "en", "English" ), ( "es", "Spanish" ), ( "fr", "French" ) ]
        , name = Just "language"
        , label = Just "Language"
        , placeholder = Just "Choose a language"
        , hint = Just "Used for localization"
        , help = Nothing
        , isRequired = True
        , drag = Drag.idle
        }


radioField : Element
radioField =
    FieldElement
        { id = Id.unset
        , field = Radio [ ( "yes", "Yes" ), ( "no", "No" ) ]
        , name = Just "contact_ok"
        , label = Just "May we contact you?"
        , placeholder = Nothing
        , hint = Nothing
        , help = Nothing
        , isRequired = True
        , drag = Drag.idle
        }


groupElement : Element
groupElement =
    ElementGroup
        { id = Id.unset
        , name = Just "person"
        , label = Just "Person"
        , inline = True
        , elements = [ textField ]
        , isOpen = True
        , drag = Drag.idle
        }


repeatableElement : Element
repeatableElement =
    RepeatableGroup
        { id = Id.unset
        , name = Just "addresses"
        , label = Just "Addresses"
        , inline = False
        , elements = [ textField ]
        , isOpen = True
        , drag = Drag.idle
        }


reviewElement : Element
reviewElement =
    Review
        { id = Id.unset
        , name = Just "summary"
        , text = Just "Please review your details before submitting"
        , drag = Drag.idle
        }


helpElement : Element
helpElement =
    Help
        { id = Id.unset
        , name = Just "help_text"
        , button = Just "Need help?"
        , text = Just "Contact support@example.com"
        , drag = Drag.idle
        }


minimalTextField : Element
minimalTextField =
    FieldElement
        { id = Id.unset
        , field = TextField
        , name = Nothing
        , label = Nothing
        , placeholder = Nothing
        , hint = Nothing
        , help = Nothing
        , isRequired = False
        , drag = Drag.idle
        }


emptySelectField : Element
emptySelectField =
    FieldElement
        { id = Id.unset
        , field = Select []
        , name = Just "empty"
        , label = Just "Empty"
        , placeholder = Nothing
        , hint = Nothing
        , help = Nothing
        , isRequired = False
        , drag = Drag.idle
        }


blankRangeField : Element
blankRangeField =
    FieldElement
        { id = Id.unset
        , field = IntegerField { min = Value.blank, max = Value.blank }
        , name = Just "unbounded"
        , label = Just "Unbounded"
        , placeholder = Nothing
        , hint = Nothing
        , help = Nothing
        , isRequired = False
        , drag = Drag.idle
        }
