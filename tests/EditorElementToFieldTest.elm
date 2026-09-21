module EditorElementToFieldTest exposing (suite)

{- Tests for `FormToolkit.Field.load`: materializing the editor's canonical JSON
   into a usable FormToolkit.Field.

   The key property is that loaded fields carry their `name`, so the resulting
   form can be filled and round-tripped through the values codec
   (Parse.json / Field.updateValuesFromJson). The schema carries no ids, so
   `load` returns a form whose identifier type stays open.
-}

import Expect
import FormToolkit.Error as Error
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import Internal.Editor.Drag as Drag
import Internal.Editor.Element as Element exposing (Attribute(..), Element(..), Field(..))
import Internal.Editor.Id as Id
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class)


suite : Test
suite =
    describe "Field.load"
        [ test "a JSON with no fields is an error" <|
            \_ ->
                loadJson """{"type":"group","name":"person","fields":[]}"""
                    |> Expect.equal (Err "The JSON describes no fields")
        , test "a JSON that is not an element is an error" <|
            \_ ->
                loadJson """{"type":"nonsense"}"""
                    |> Expect.err
        , test "named text field loads with its name so values round-trip" <|
            \_ ->
                case loadElement textFieldElement of
                    Err error ->
                        Expect.fail error

                    Ok field ->
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
        , test "group loads to a nested group keyed by name" <|
            \_ ->
                case loadElement (groupElement [ textFieldElement ]) of
                    Err error ->
                        Expect.fail error

                    Ok field ->
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
        , test "repeatable group loads to a repeatable field" <|
            \_ ->
                loadElement (repeatableGroupElement [ textFieldElement ])
                    |> Result.toMaybe
                    |> Expect.notEqual Nothing
        , test "empty repeatable group is an error" <|
            \_ ->
                loadElement (repeatableGroupElement [])
                    |> Expect.equal (Err "The JSON describes no fields")
        , test "one JSON materializes the builder and a usable form" <|
            \_ ->
                let
                    json =
                        """{"type":"group","inline":false,"name":"person","label":"Person","fields":[{"type":"text","name":"first_name","label":"First name","placeholder":null,"help":null,"hint":null,"required":false}]}"""
                in
                case loadJson json of
                    Err error ->
                        Expect.fail error

                    Ok field ->
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
                case loadElement (groupElementWith { inline = True } [ textFieldElement ]) of
                    Err error ->
                        Expect.fail error

                    Ok field ->
                        field
                            |> Field.toHtml (always never)
                            |> Query.fromHtml
                            |> Query.find [ class "inline-fields" ]
                            |> Query.has [ class "inline-fields" ]
        , test "a stacked group does not render inline" <|
            \_ ->
                case loadElement (groupElement [ textFieldElement ]) of
                    Err error ->
                        Expect.fail error

                    Ok field ->
                        field
                            |> Field.toHtml (always never)
                            |> Query.fromHtml
                            |> Query.hasNot [ class "inline-fields" ]
        ]


loadJson : String -> Result String (Field.Field id)
loadJson json =
    case Decode.decodeString Decode.value json of
        Ok value ->
            Field.load value
                |> Result.mapError Error.toEnglish

        Err error ->
            Err (Decode.errorToString error)


loadElement : Element -> Result String (Field.Field id)
loadElement element =
    case Element.encode element of
        Just json ->
            Field.load json
                |> Result.mapError Error.toEnglish

        Nothing ->
            Err "the element is not encodable"



-- Fixtures


textFieldElement : Element
textFieldElement =
    FieldElement
        { id = Id.fromInt 1
        , field = TextField
        , attributes =
            [ Name "first_name"
            , Label "First name"
            ]
        , drag = Drag.idle
        }


groupElement : List Element -> Element
groupElement elements =
    groupElementWith { inline = False } elements


groupElementWith : { inline : Bool } -> List Element -> Element
groupElementWith { inline } elements =
    ElementGroup
        { id = Id.fromInt 2
        , attributes =
            [ Name "person"
            , Label "Person"
            ]
                ++ (if inline then
                        [ Inline True ]

                    else
                        []
                   )
        , elements = elements
        , drag = Drag.idle
        , isOpen = True
        }


repeatableGroupElement : List Element -> Element
repeatableGroupElement elements =
    RepeatableGroup
        { id = Id.fromInt 3
        , attributes =
            [ Name "people"
            , Label "People"
            ]
        , elements = elements
        , drag = Drag.idle
        , isOpen = True
        }
