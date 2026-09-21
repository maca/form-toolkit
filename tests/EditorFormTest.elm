module EditorFormTest exposing (suite)

{-| `labelInputId` is the contract `Builder` focuses by, so it must name the
Label input of every form that has one.
-}

import Expect exposing (Expectation)
import FormToolkit.Value as Value
import Html.Attributes as Attrs
import Internal.Editor.Drag as Drag
import Internal.Editor.Element as Element exposing (Attribute(..), Element(..), Field(..))
import Internal.Editor.Form as EditorForm
import Internal.Editor.Id as Id
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, id, tag, text)


suite : Test
suite =
    describe "EditorForm"
        [ describe "labelInputId names the Label input"
            [ test "text field" <| \_ -> labelInputHasLabel "Label" textField
            , test "checkbox field" <| \_ -> labelInputHasLabel "Label" checkboxField
            , test "integer field" <| \_ -> labelInputHasLabel "Label" integerField
            , test "select field" <| \_ -> labelInputHasLabel "Label" selectField
            , test "group" <| \_ -> labelInputHasLabel "Label" groupElement
            , test "repeatable group" <| \_ -> labelInputHasLabel "Label" repeatableElement
            , test "help labels its button text input" <| \_ -> labelInputHasLabel "Button Text" helpElement
            ]
        , describe "isValid"
            [ test "labeled text field" <| \_ -> Expect.equal True (EditorForm.isValid textField)
            , test "unlabeled field" <| \_ -> Expect.equal False (EditorForm.isValid unlabeledField)
            , test "group" <| \_ -> Expect.equal True (EditorForm.isValid groupElement)
            , test "repeatable group" <| \_ -> Expect.equal True (EditorForm.isValid repeatableElement)
            , test "select with one option" <| \_ -> Expect.equal True (EditorForm.isValid selectField)
            , test "select without options" <| \_ -> Expect.equal False (EditorForm.isValid (Element.select []))
            , test "review" <| \_ -> Expect.equal True (EditorForm.isValid reviewElement)
            , test "help" <| \_ -> Expect.equal True (EditorForm.isValid helpElement)
            ]
        , describe "forms without a Label"
            [ test "review" <| \_ -> Expect.equal Nothing (EditorForm.labelInputId reviewElement)
            , test "blank" <| \_ -> Expect.equal Nothing (EditorForm.labelInputId (Blank Id.unset))
            ]
        ]


labelInputHasLabel : String -> Element -> Expectation
labelInputHasLabel labelText element =
    case EditorForm.labelInputId element of
        Nothing ->
            Expect.fail "expected the form to have a label input"

        Just inputId ->
            element
                |> EditorForm.init
                |> EditorForm.view
                |> Query.fromHtml
                |> Expect.all
                    [ Query.find [ tag "label", attribute (Attrs.for inputId) ]
                        >> Query.has [ text labelText ]
                    , Query.find [ tag "input", id inputId ]
                        >> Query.has [ attribute (Attrs.name "editor-label") ]
                    ]



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


unlabeledField : Element
unlabeledField =
    FieldElement
        { id = Id.unset
        , field = TextField
        , attributes = [ Name "text_field" ]
        , drag = Drag.idle
        }


checkboxField : Element
checkboxField =
    FieldElement
        { id = Id.unset
        , field = Checkbox
        , attributes =
            [ Name "subscribe"
            , Label "Subscribe"
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
            , Required True
            ]
        , drag = Drag.idle
        }


selectField : Element
selectField =
    FieldElement
        { id = Id.unset
        , field = Select [ ( "en", "English" ) ]
        , attributes =
            [ Name "language"
            , Label "Language"
            , Required True
            ]
        , drag = Drag.idle
        }


groupElement : Element
groupElement =
    ElementGroup
        { id = Id.unset
        , attributes =
            [ Name "address"
            , Label "Address"
            ]
        , elements = [ textField ]
        , isOpen = True
        , drag = Drag.idle
        }


repeatableElement : Element
repeatableElement =
    RepeatableGroup
        { id = Id.unset
        , attributes =
            [ Name "people"
            , Label "People"
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
            [ Name "review"
            , Text "Please review"
            ]
        , drag = Drag.idle
        }


helpElement : Element
helpElement =
    Help
        { id = Id.unset
        , attributes =
            [ Name "help"
            , Button "Help"
            , Text "Some help"
            ]
        , drag = Drag.idle
        }
