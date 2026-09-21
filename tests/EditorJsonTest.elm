module EditorJsonTest exposing (suite)

{-| Tests for `FormToolkit.Editor.save` and `FormToolkit.Editor.load`: the
facade's serialization pair, and the JSON shape it pins.
-}

import Expect
import FormToolkit.Editor as Editor
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)


defaultEditorJson : String
defaultEditorJson =
    -- also the expectation in the `Editor.save` documentation snippet
    """{"type":"group","fields":[{"type":"group","fields":[{"type":"text","name":"text_field"}],"name":"fields"}],"name":"root"}"""


suite : Test
suite =
    describe "FormToolkit.Editor JSON"
        [ test "saving a fresh editor gives a form with one text field" <|
            \_ ->
                Editor.save Editor.init
                    |> Maybe.map (Encode.encode 0)
                    |> Expect.equal (Just defaultEditorJson)
        , test "loading what was saved gives the same form" <|
            \_ ->
                defaultEditorJson
                    |> Decode.decodeString Editor.load
                    |> Result.map Editor.save
                    |> Result.map (Maybe.map (Encode.encode 0))
                    |> Expect.equal (Ok (Just defaultEditorJson))
        , test "a form of one field round-trips" <|
            \_ ->
                let
                    json =
                        """{"type":"text","name":"first_name","label":"First name"}"""
                in
                json
                    |> Decode.decodeString Editor.load
                    |> Result.map Editor.save
                    |> Result.map (Maybe.map (Encode.encode 0))
                    |> Expect.equal (Ok (Just json))
        ]
