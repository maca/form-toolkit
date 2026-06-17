module EditorForm exposing (EditorForm, Msg, init, update, view)

import Dict exposing (Dict)
import Editor.Element as Element exposing (Element(..), Field(..))
import FormToolkit.Field as FormField exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value exposing (Value)
import Html exposing (Html)


type FieldId
    = NameId
    | LabelId
    | PlaceholderId
    | HintId
    | HelpId
    | RequiredId
    | MinId
    | MaxId
    | InlineId
    | OptionsId
    | OptionValueId
    | OptionLabelId


type alias EditorForm =
    { element : Element
    , field : Field FieldId
    }


type Msg
    = FieldMsg (FormField.Msg FieldId)


init : Element -> EditorForm
init element =
    { element = element
    , field = fromElement element
    }


update : Msg -> EditorForm -> ( EditorForm, Maybe Element )
update msg form =
    case msg of
        FieldMsg fieldMsg ->
            let
                newField = FormField.update fieldMsg form.field
                updatedElement = parseElement form.element newField
            in
            ( { form | field = newField }, Just updatedElement )


view : EditorForm -> Html Msg
view form =
    FormField.toHtml FieldMsg form.field


fromElement : Element -> Field FieldId
fromElement element =
    case element of
        FieldElement params ->
            fromFieldElement params

        ElementGroup params ->
            fromGroupElement params

        RepeatableGroup params ->
            fromRepeatableGroupElement params

        Review params ->
            reviewForm params

        Help params ->
            helpForm params

        Blank _ ->
            FormField.text [ FormField.label "Placeholder" ]


fromFieldElement : Element.FieldParams -> Field FieldId
fromFieldElement params =
    case params.field of
        TextField ->
            textFieldForm params

        Checkbox ->
            checkboxFieldForm params

        IntegerField { min, max } ->
            rangeFieldForm params
                (FormField.int [ FormField.identifier MinId, FormField.value min ])
                (FormField.int [ FormField.identifier MaxId, FormField.value max ])

        DateField { min, max } ->
            rangeFieldForm params
                (FormField.date [ FormField.identifier MinId, FormField.value min ])
                (FormField.date [ FormField.identifier MaxId, FormField.value max ])

        MonthField { min, max } ->
            rangeFieldForm params
                (FormField.month [ FormField.identifier MinId, FormField.value min ])
                (FormField.month [ FormField.identifier MaxId, FormField.value max ])

        Select options ->
            optionsFieldForm params options

        Radio options ->
            optionsFieldForm params options


textFieldForm : Element.FieldParams -> Field FieldId
textFieldForm params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string params.name)
            , FormField.required True
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (getLocaleValue params.label)
            , FormField.hint "If not provided **Field Name** will be used"
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (getLocaleValue params.placeholder)
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (getLocaleValue params.hint)
            ]
        , FormField.checkbox
            [ FormField.identifier RequiredId
            , FormField.label "Is Required?"
            , FormField.value (Value.bool params.isRequired)
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Field Help"
            , FormField.value (getLocaleValue params.help)
            ]
        ]


checkboxFieldForm : Element.FieldParams -> Field FieldId
checkboxFieldForm params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string params.name)
            , FormField.required True
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (getLocaleValue params.label)
            , FormField.hint "If not provided **Field Name** will be used"
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (getLocaleValue params.placeholder)
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (getLocaleValue params.hint)
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Field Help"
            , FormField.value (getLocaleValue params.help)
            ]
        ]


rangeFieldForm : Element.FieldParams -> Field FieldId -> Field FieldId -> Field FieldId
rangeFieldForm params minField maxField =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string params.name)
            , FormField.required True
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (getLocaleValue params.label)
            , FormField.hint "If not provided **Field Name** will be used"
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (getLocaleValue params.placeholder)
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (getLocaleValue params.hint)
            ]
        , FormField.group []
            [ minField, maxField ]
        , FormField.checkbox
            [ FormField.identifier RequiredId
            , FormField.label "Is Required?"
            , FormField.value (Value.bool params.isRequired)
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Field Help"
            , FormField.value (getLocaleValue params.help)
            ]
        ]


optionsFieldForm : Element.FieldParams -> List ( String, Dict String String ) -> Field FieldId
optionsFieldForm params options =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string params.name)
            , FormField.required True
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (getLocaleValue params.label)
            , FormField.hint "If not provided **Field Name** will be used"
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (getLocaleValue params.placeholder)
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (getLocaleValue params.hint)
            ]
        , FormField.checkbox
            [ FormField.identifier RequiredId
            , FormField.label "Is Required?"
            , FormField.value (Value.bool params.isRequired)
            ]
        , FormField.repeatable
            [ FormField.identifier OptionsId
            , FormField.label "Options"
            ]
            optionTemplate
            (List.map optionInitializer options)
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Field Help"
            , FormField.value (getLocaleValue params.help)
            ]
        ]


optionTemplate : Field FieldId
optionTemplate =
    FormField.group []
        [ FormField.text
            [ FormField.identifier OptionValueId
            , FormField.label "Option Value"
            , FormField.required True
            ]
        , FormField.text
            [ FormField.identifier OptionLabelId
            , FormField.label "Option Label"
            , FormField.required True
            ]
        ]


optionInitializer : ( String, Dict String String ) -> (Field FieldId -> Field FieldId)
optionInitializer ( optionValue, labels ) =
    \template ->
        template
            |> FormField.updateWithId OptionValueId (FormField.value (Value.string optionValue))
            |> FormField.updateWithId OptionLabelId (FormField.value (getLocaleValueFromDict labels))


fromGroupElement : { a | name : String, label : Dict String String, inline : Bool } -> Field FieldId
fromGroupElement params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string params.name)
            , FormField.required True
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (getLocaleValue params.label)
            ]
        , FormField.select
            [ FormField.identifier InlineId
            , FormField.label "Format"
            , FormField.options
                [ ( "Stacked", Value.bool False )
                , ( "Inline", Value.bool True )
                ]
            , FormField.value (Value.bool params.inline)
            , FormField.required True
            ]
        ]


fromRepeatableGroupElement : { a | name : String, label : Dict String String, inline : Bool } -> Field FieldId
fromRepeatableGroupElement params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string params.name)
            , FormField.required True
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (getLocaleValue params.label)
            ]
        , FormField.select
            [ FormField.identifier InlineId
            , FormField.label "Format"
            , FormField.options
                [ ( "Stacked", Value.bool False )
                , ( "Inline", Value.bool True )
                ]
            , FormField.value (Value.bool params.inline)
            , FormField.required True
            ]
        ]


reviewForm : { a | name : String, text : Dict String String } -> Field FieldId
reviewForm params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string params.name)
            , FormField.required True
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Text"
            , FormField.value (getLocaleValue params.text)
            , FormField.required True
            ]
        ]


helpForm : { a | name : String, button : Dict String String, text : Dict String String } -> Field FieldId
helpForm params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string params.name)
            , FormField.required True
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Text"
            , FormField.value (getLocaleValue params.text)
            , FormField.required True
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Button Text"
            , FormField.value (getLocaleValue params.button)
            , FormField.required True
            ]
        ]


parseElement : Element -> Field FieldId -> Element
parseElement element field =
    case element of
        FieldElement params ->
            FieldElement
                { params
                    | name = getStringValue NameId field params.name
                    , label = setLocaleValue LabelId field params.label
                    , placeholder = setLocaleValue PlaceholderId field params.placeholder
                    , hint = setLocaleValue HintId field params.hint
                    , help = setLocaleValue HelpId field params.help
                    , isRequired = getBoolValue RequiredId field params.isRequired
                    , field = parseFieldType params.field field
                }

        ElementGroup params ->
            ElementGroup
                { params
                    | name = getStringValue NameId field params.name
                    , label = setLocaleValue LabelId field params.label
                    , inline = getBoolValue InlineId field params.inline
                }

        RepeatableGroup params ->
            RepeatableGroup
                { params
                    | name = getStringValue NameId field params.name
                    , label = setLocaleValue LabelId field params.label
                    , inline = getBoolValue InlineId field params.inline
                }

        Review params ->
            Review
                { params
                    | name = getStringValue NameId field params.name
                    , text = setLocaleValue HelpId field params.text
                }

        Help params ->
            Help
                { params
                    | name = getStringValue NameId field params.name
                    , text = setLocaleValue HelpId field params.text
                    , button = setLocaleValue LabelId field params.button
                }

        _ ->
            element


parseFieldType : Element.Field -> Field FieldId -> Element.Field
parseFieldType fieldType field =
    case fieldType of
        IntegerField _ ->
            IntegerField
                { min = getValueByIdWithDefault MinId field Value.blank
                , max = getValueByIdWithDefault MaxId field Value.blank
                }

        DateField _ ->
            DateField
                { min = getValueByIdWithDefault MinId field Value.blank
                , max = getValueByIdWithDefault MaxId field Value.blank
                }

        MonthField _ ->
            MonthField
                { min = getValueByIdWithDefault MinId field Value.blank
                , max = getValueByIdWithDefault MaxId field Value.blank
                }

        Select _ ->
            Select (parseOptions field)

        Radio _ ->
            Radio (parseOptions field)

        _ ->
            fieldType


getStringValue : FieldId -> Field FieldId -> String -> String
getStringValue id field default =
    Parse.parse (Parse.field id (Parse.maybe Parse.string)) field
        |> Result.toMaybe
        |> Maybe.andThen identity
        |> Maybe.withDefault default


getBoolValue : FieldId -> Field FieldId -> Bool -> Bool
getBoolValue id field default =
    Parse.parse (Parse.field id (Parse.maybe Parse.bool)) field
        |> Result.toMaybe
        |> Maybe.andThen identity
        |> Maybe.withDefault default


getValueByIdWithDefault : FieldId -> Field FieldId -> Value -> Value
getValueByIdWithDefault id field default =
    Parse.parse (Parse.field id (Parse.maybe Parse.value)) field
        |> Result.toMaybe
        |> Maybe.andThen identity
        |> Maybe.withDefault default


getLocaleValue : Dict String String -> Value
getLocaleValue dict =
    Dict.values dict
        |> List.head
        |> Maybe.map Value.string
        |> Maybe.withDefault Value.blank


getLocaleValueFromDict : Dict String String -> Value
getLocaleValueFromDict dict =
    getLocaleValue dict


setLocaleValue : FieldId -> Field FieldId -> Dict String String -> Dict String String
setLocaleValue id field dict =
    case Parse.parse (Parse.field id (Parse.maybe Parse.string)) field of
        Ok (Just str) ->
            Dict.values dict
                |> List.head
                |> Maybe.map (\locale -> Dict.singleton locale str)
                |> Maybe.withDefault dict

        Ok Nothing ->
            dict

        Err _ ->
            dict


parseOptions : Field FieldId -> List ( String, Dict String String )
parseOptions field =
    Parse.parse
        (Parse.field OptionsId (Parse.list (optionParser "")))
        field
        |> Result.withDefault []


optionParser : String -> Parse.Parser FieldId ( String, Dict String String )
optionParser locale =
    Parse.map2 (\v l -> ( v, Dict.singleton locale l ))
        (Parse.field OptionValueId (Parse.maybe Parse.string |> Parse.map (Maybe.withDefault "")))
        (Parse.field OptionLabelId (Parse.maybe Parse.string |> Parse.map (Maybe.withDefault "")))
