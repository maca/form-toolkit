module EditorElementToFieldTest exposing (suite)

{- Tests for Editor.Element.toField: materializing an element tree into a
usable FormToolkit.Field.

The key property is that materialized fields carry their `name` (and the
element `Id` as identifier), so the resulting form can be filled and
round-tripped through the values codec (Parse.json / Field.updateValuesFromJson).
-}

import Editor.Drag as Drag
import Editor.Element as Element exposing (Element(..), Field(..))
import Editor.Id as Id
import Expect
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class)


suite : Test
suite =
    describe "Editor.Element.toField"
        [ test "Blank materializes to Nothing" <|
            \_ ->
                Element.toField (Blank (Id.fromInt 0))
                    |> Expect.equal Nothing
        , test "empty group materializes to Nothing" <|
            \_ ->
                Element.toField (groupElement [])
                    |> Expect.equal Nothing
        , test "named text field materializes with its name so values round-trip" <|
            \_ ->
                case Element.toField textFieldElement of
                    Nothing ->
                        Expect.fail "expected a field"

                    Just field ->
                        Field.group [] [ field ]
                            |> Field.updateValuesFromJson
                                (Encode.object [ ( "first_name", Encode.string "Frank" ) ])
                            |> Result.andThen (Parse.parse Parse.json)
                            |> Result.mapError (always "codec failed")
                            |> Result.andThen
                                (Decode.decodeValue (Decode.field "first_name" Decode.string)
                                    >> Result.mapError Decode.errorToString
                                )
                            |> Expect.equal (Ok "Frank")
        , test "group materializes to a nested group keyed by name" <|
            \_ ->
                case Element.toField (groupElement [ textFieldElement ]) of
                    Nothing ->
                        Expect.fail "expected a group"

                    Just field ->
                        Field.group [] [ field ]
                            |> Field.updateValuesFromJson
                                (Encode.object
                                    [ ( "person", Encode.object [ ( "first_name", Encode.string "Frank" ) ] ) ]
                                )
                            |> Result.andThen (Parse.parse Parse.json)
                            |> Result.mapError (always "codec failed")
                            |> Result.andThen
                                (Decode.decodeValue (Decode.at [ "person", "first_name" ] Decode.string)
                                    >> Result.mapError Decode.errorToString
                                )
                            |> Expect.equal (Ok "Frank")
        , test "repeatable group materializes to a repeatable field" <|
            \_ ->
                Element.toField (repeatableGroupElement [ textFieldElement ])
                    |> Expect.notEqual Nothing
        , test "empty repeatable group materializes to Nothing" <|
            \_ ->
                Element.toField (repeatableGroupElement [])
                    |> Expect.equal Nothing
        , test "one JSON materializes the builder and a usable form" <|
            \_ ->
                let
                    json =
                        """{"type":"group","inline":false,"name":"person","label":"Person","fields":[{"type":"text","name":"first_name","label":"First name","placeholder":null,"help":null,"hint":null,"required":false}]}"""
                in
                case Decode.decodeString Element.decode json of
                    Err error ->
                        Expect.fail (Decode.errorToString error)

                    Ok element ->
                        case Element.toField element of
                            Nothing ->
                                Expect.fail "expected a form"

                            Just field ->
                                Field.group [] [ field ]
                                    |> Field.updateValuesFromJson
                                        (Encode.object
                                            [ ( "person", Encode.object [ ( "first_name", Encode.string "Frank" ) ] ) ]
                                        )
                                    |> Result.andThen (Parse.parse Parse.json)
                                    |> Result.mapError (always "codec failed")
                                    |> Result.andThen
                                        (Decode.decodeValue (Decode.at [ "person", "first_name" ] Decode.string)
                                            >> Result.mapError Decode.errorToString
                                        )
                                    |> Expect.equal (Ok "Frank")
        , test "an inline group renders its fields inline" <|
            \_ ->
                case Element.toField (groupElementWith { inline = True } [ textFieldElement ]) of
                    Nothing ->
                        Expect.fail "expected a group"

                    Just field ->
                        field
                            |> Field.toHtml (always never)
                            |> Query.fromHtml
                            |> Query.find [ class "inline-fields" ]
                            |> Query.has [ class "inline-fields" ]
        , test "a stacked group does not render inline" <|
            \_ ->
                case Element.toField (groupElement [ textFieldElement ]) of
                    Nothing ->
                        Expect.fail "expected a group"

                    Just field ->
                        field
                            |> Field.toHtml (always never)
                            |> Query.fromHtml
                            |> Query.hasNot [ class "inline-fields" ]
        ]



-- Fixtures


textFieldElement : Element
textFieldElement =
    FieldElement
        { id = Id.fromInt 1
        , field = TextField
        , name = Just "first_name"
        , label = Just "First name"
        , placeholder = Nothing
        , hint = Nothing
        , help = Nothing
        , isRequired = False
        , drag = Drag.idle
        }


groupElement : List Element -> Element
groupElement elements =
    groupElementWith { inline = False } elements


groupElementWith : { inline : Bool } -> List Element -> Element
groupElementWith { inline } elements =
    ElementGroup
        { id = Id.fromInt 2
        , name = Just "person"
        , label = Just "Person"
        , inline = inline
        , elements = elements
        , drag = Drag.idle
        , isOpen = True
        }


repeatableGroupElement : List Element -> Element
repeatableGroupElement elements =
    RepeatableGroup
        { id = Id.fromInt 3
        , name = Just "people"
        , label = Just "People"
        , inline = False
        , elements = elements
        , drag = Drag.idle
        , isOpen = True
        }
