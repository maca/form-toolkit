module EditorElementJsonTest exposing (suite)

{- Tests for the editor (form-definition) JSON codec: the round-trip pair
   Editor.Element.encode / Editor.Element.decode, and the raw JSON they pin.

   Not serialized: ids, drag state, and whether groups are open — a decoded
   tree is fully expanded.

   Fixtures are compared with (==), so their attribute lists follow the order
   decode produces: Name, Label, Placeholder, Hint, HelpText, Button, Text,
   Required, Inline.
-}

import Expect
import FormToolkit.Value as Value
import Internal.Editor.Drag as Drag
import Internal.Editor.Element as Element exposing (Attribute(..), Element(..), Field(..))
import Internal.Editor.Id as Id
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
            [ test "a field with no attributes round-trips" <| \_ -> roundTrip minimalTextField
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
        , describe "the encoded JSON is pinned (the external contract)"
            [ test "a field with every kind of attribute" <|
                \_ ->
                    textField
                        |> encodedJson
                        |> Expect.equal
                            (Just
                                """{"type":"text","name":"first_name","label":"First name","placeholder":"e.g. Frank","hint":"Your given name","help":"As it appears on your passport","required":true}"""
                            )
            , test "an unset attribute produces no key" <|
                \_ ->
                    minimalTextField
                        |> encodedJson
                        |> Expect.equal (Just """{"type":"text"}""")
            , test "a ranged field carries min and max" <|
                \_ ->
                    blankRangeField
                        |> encodedJson
                        |> Expect.equal
                            (Just
                                """{"type":"integer","min":"","max":"","name":"unbounded","label":"Unbounded"}"""
                            )
            , test "a group carries its fields and its own attributes" <|
                \_ ->
                    groupElement
                        |> encodedJson
                        |> Expect.equal
                            (Just
                                """{"type":"group","fields":[{"type":"text"}],"name":"person","label":"Person","inline":true}"""
                            )
            ]
        , describe "decode tolerance and strictness"
            [ test "explicit nulls and absent keys mean the same thing" <|
                \_ ->
                    """{"type":"text","name":"first_name","label":null,"placeholder":null,"hint":null,"help":null,"required":false}"""
                        |> Decode.decodeString Element.decode
                        |> Expect.equal
                            (Ok
                                (FieldElement
                                    { id = Id.unset
                                    , field = TextField
                                    , attributes = [ Name "first_name", Required False ]
                                    , drag = Drag.idle
                                    }
                                )
                            )
            , test "an attribute of the wrong type fails the decode" <|
                \_ ->
                    """{"type":"text","name":5}"""
                        |> Decode.decodeString Element.decode
                        |> Result.toMaybe
                        |> Expect.equal Nothing
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


encodedJson : Element -> Maybe String
encodedJson element =
    element
        |> Element.encode
        |> Maybe.map (Encode.encode 0)



-- Element fixtures


textField : Element
textField =
    FieldElement
        { id = Id.unset
        , field = TextField
        , attributes =
            [ Name "first_name"
            , Label "First name"
            , Placeholder "e.g. Frank"
            , Hint "Your given name"
            , HelpText "As it appears on your passport"
            , Required True
            ]
        , drag = Drag.idle
        }


checkboxField : Element
checkboxField =
    FieldElement
        { id = Id.unset
        , field = Checkbox
        , attributes =
            [ Name "subscribe"
            , Label "Subscribe to the newsletter"
            , Hint "We send at most one email a month"
            , HelpText "You can unsubscribe at any time"
            ]
        , drag = Drag.idle
        }


integerField : Element
integerField =
    FieldElement
        { id = Id.unset
        , field = IntegerField { min = Value.int 0, max = Value.int 100 }
        , attributes =
            [ Name "age"
            , Label "Age"
            , Placeholder "42"
            , Hint "Your age in years"
            , HelpText "Must be at least 18"
            , Required True
            ]
        , drag = Drag.idle
        }


dateField : Element
dateField =
    FieldElement
        { id = Id.unset
        , field = DateField { min = Value.date (Time.millisToPosix 0), max = Value.date (Time.millisToPosix 0) }
        , attributes =
            [ Name "birth_date"
            , Label "Birth date"
            , Hint "When were you born?"
            , HelpText "Used to calculate age"
            , Required True
            ]
        , drag = Drag.idle
        }


monthField : Element
monthField =
    FieldElement
        { id = Id.unset
        , field = MonthField { min = Value.month (Time.millisToPosix 0), max = Value.month (Time.millisToPosix 0) }
        , attributes =
            [ Name "start_month"
            , Label "Start month"
            ]
        , drag = Drag.idle
        }


selectField : Element
selectField =
    FieldElement
        { id = Id.unset
        , field = Select [ ( "en", "English" ), ( "es", "Spanish" ), ( "fr", "French" ) ]
        , attributes =
            [ Name "language"
            , Label "Language"
            , Placeholder "Choose a language"
            , Hint "Used for localization"
            , Required True
            ]
        , drag = Drag.idle
        }


radioField : Element
radioField =
    FieldElement
        { id = Id.unset
        , field = Radio [ ( "yes", "Yes" ), ( "no", "No" ) ]
        , attributes =
            [ Name "contact_ok"
            , Label "May we contact you?"
            , Required True
            ]
        , drag = Drag.idle
        }


groupElement : Element
groupElement =
    ElementGroup
        { id = Id.unset
        , attributes =
            [ Name "person"
            , Label "Person"
            , Inline True
            ]
        , elements = [ minimalTextField ]
        , isOpen = True
        , drag = Drag.idle
        }


repeatableElement : Element
repeatableElement =
    RepeatableGroup
        { id = Id.unset
        , attributes =
            [ Name "addresses"
            , Label "Addresses"
            ]
        , elements = [ textField ]
        , isOpen = True
        , drag = Drag.idle
        }


reviewElement : Element
reviewElement =
    Review
        { id = Id.unset
        , attributes =
            [ Name "summary"
            , Text "Please review your details before submitting"
            ]
        , drag = Drag.idle
        }


helpElement : Element
helpElement =
    Help
        { id = Id.unset
        , attributes =
            [ Name "help_text"
            , Button "Need help?"
            , Text "Contact support@example.com"
            ]
        , drag = Drag.idle
        }


minimalTextField : Element
minimalTextField =
    FieldElement
        { id = Id.unset
        , field = TextField
        , attributes = []
        , drag = Drag.idle
        }


emptySelectField : Element
emptySelectField =
    FieldElement
        { id = Id.unset
        , field = Select []
        , attributes =
            [ Name "empty"
            , Label "Empty"
            ]
        , drag = Drag.idle
        }


blankRangeField : Element
blankRangeField =
    FieldElement
        { id = Id.unset
        , field = IntegerField { min = Value.blank, max = Value.blank }
        , attributes =
            [ Name "unbounded"
            , Label "Unbounded"
            ]
        , drag = Drag.idle
        }
