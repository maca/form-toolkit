module EditorForm exposing (EditorForm, Msg, init, isValid, update, view)

import Editor.Element as Element exposing (Element(..), Field(..))
import FormToolkit.Field as FormField exposing (Field)
import FormToolkit.Error exposing (Error)
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


isValid : Element -> Bool
isValid element =
    case parseElement element (fromElement element) of
        Ok _ ->
            True

        Err _ ->
            False


update : Msg -> EditorForm -> ( EditorForm, Result (Error FieldId) Element )
update msg form =
    case msg of
        FieldMsg fieldMsg ->
            let
                newField = FormField.update fieldMsg form.field
                updatedElement = parseElement form.element newField
            in
            ( { form | field = newField, element = Result.withDefault form.element updatedElement }, updatedElement )


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
                (FormField.text [ FormField.identifier MinId, FormField.label "Min", FormField.value (Value.string (Maybe.withDefault "" (Value.toString min))) ])
                (FormField.text [ FormField.identifier MaxId, FormField.label "Max", FormField.value (Value.string (Maybe.withDefault "" (Value.toString max))) ])

        DateField { min, max } ->
            rangeFieldForm params
                (FormField.text [ FormField.identifier MinId, FormField.label "Min", FormField.value (Value.string (Maybe.withDefault "" (Value.toString min))) ])
                (FormField.text [ FormField.identifier MaxId, FormField.label "Max", FormField.value (Value.string (Maybe.withDefault "" (Value.toString max))) ])

        MonthField { min, max } ->
            rangeFieldForm params
                (FormField.text [ FormField.identifier MinId, FormField.label "Min", FormField.value (Value.string (Maybe.withDefault "" (Value.toString min))) ])
                (FormField.text [ FormField.identifier MaxId, FormField.label "Max", FormField.value (Value.string (Maybe.withDefault "" (Value.toString max))) ])

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
            , FormField.value (Value.string (Maybe.withDefault "" params.name))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" params.label))
            , FormField.required True
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (Value.string (Maybe.withDefault "" params.placeholder))
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (Value.string (Maybe.withDefault "" params.hint))
            ]
        , FormField.checkbox
            [ FormField.identifier RequiredId
            , FormField.label "Is Required?"
            , FormField.value (Value.bool params.isRequired)
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Field Help"
            , FormField.value (Value.string (Maybe.withDefault "" params.help))
            ]
        ]


checkboxFieldForm : Element.FieldParams -> Field FieldId
checkboxFieldForm params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" params.name))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" params.label))
            , FormField.required True
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (Value.string (Maybe.withDefault "" params.placeholder))
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (Value.string (Maybe.withDefault "" params.hint))
            ]
        , FormField.checkbox
            [ FormField.identifier RequiredId
            , FormField.label "Is Required?"
            , FormField.value (Value.bool params.isRequired)
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Field Help"
            , FormField.value (Value.string (Maybe.withDefault "" params.help))
            ]
        ]


rangeFieldForm : Element.FieldParams -> Field FieldId -> Field FieldId -> Field FieldId
rangeFieldForm params minField maxField =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" params.name))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" params.label))
            , FormField.required True
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (Value.string (Maybe.withDefault "" params.placeholder))
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (Value.string (Maybe.withDefault "" params.hint))
            ]
        , minField
        , maxField
        , FormField.checkbox
            [ FormField.identifier RequiredId
            , FormField.label "Is Required?"
            , FormField.value (Value.bool params.isRequired)
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Field Help"
            , FormField.value (Value.string (Maybe.withDefault "" params.help))
            ]
        ]


optionsFieldForm : Element.FieldParams -> List ( String, String ) -> Field FieldId
optionsFieldForm params options =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" params.name))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" params.label))
            , FormField.required True
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (Value.string (Maybe.withDefault "" params.placeholder))
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (Value.string (Maybe.withDefault "" params.hint))
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
            , FormField.value (Value.string (Maybe.withDefault "" params.help))
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


optionInitializer : ( String, String ) -> (Field FieldId -> Field FieldId)
optionInitializer ( optionValue, label_ ) =
    \template ->
        template
            |> FormField.updateWithId OptionValueId (FormField.value (Value.string optionValue))
            |> FormField.updateWithId OptionLabelId (FormField.value (Value.string label_))


fromGroupElement : { a | name : Maybe String, label : Maybe String, inline : Bool } -> Field FieldId
fromGroupElement params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" params.name))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" params.label))
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


fromRepeatableGroupElement : { a | name : Maybe String, label : Maybe String, inline : Bool } -> Field FieldId
fromRepeatableGroupElement params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" params.name))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" params.label))
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


reviewForm : { a | name : Maybe String, text : Maybe String } -> Field FieldId
reviewForm params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" params.name))
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Text"
            , FormField.value (Value.string (Maybe.withDefault "" params.text))
            ]
        ]


helpForm : { a | name : Maybe String, button : Maybe String, text : Maybe String } -> Field FieldId
helpForm params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" params.name))
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Text"
            , FormField.value (Value.string (Maybe.withDefault "" params.text))
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.label "Button Text"
            , FormField.value (Value.string (Maybe.withDefault "" params.button))
            ]
        ]


parseElement : Element -> Field FieldId -> Result (Error FieldId) Element
parseElement element field =
    case element of
        FieldElement params ->
            let
                required =
                    getBoolValue RequiredId field params.isRequired

                hint =
                    Parse.parse (Parse.field HintId (Parse.maybe Parse.string)) field
                        |> Result.withDefault params.hint

                help =
                    Parse.parse (Parse.field HelpId (Parse.maybe Parse.string)) field
                        |> Result.withDefault params.help
            in
            Parse.parse
                (Parse.map3
                    (\name label placeholder ->
                        FieldElement
                            { params
                                | name = name
                                , label = label
                                , placeholder = placeholder
                                , hint = hint
                                , help = help
                                , isRequired = required
                                , field = parseFieldType params.field field
                            }
                    )
                    (Parse.field NameId (Parse.maybe Parse.string))
                    (Parse.field LabelId (Parse.maybe Parse.string))
                    (Parse.field PlaceholderId (Parse.maybe Parse.string))
                )
                field

        ElementGroup params ->
            Parse.parse
                (Parse.map3
                    (\name label inline ->
                        ElementGroup
                            { params
                                | name = name
                                , label = label
                                , inline = inline
                            }
                    )
                    (Parse.field NameId (Parse.maybe Parse.string))
                    (Parse.field LabelId (Parse.maybe Parse.string))
                    (Parse.maybe (Parse.field InlineId Parse.bool) |> Parse.map (Maybe.withDefault params.inline))
                )
                field

        RepeatableGroup params ->
            Parse.parse
                (Parse.map3
                    (\name label inline ->
                        RepeatableGroup
                            { params
                                | name = name
                                , label = label
                                , inline = inline
                            }
                    )
                    (Parse.field NameId (Parse.maybe Parse.string))
                    (Parse.field LabelId (Parse.maybe Parse.string))
                    (Parse.maybe (Parse.field InlineId Parse.bool) |> Parse.map (Maybe.withDefault params.inline))
                )
                field

        Review params ->
            Parse.parse
                (Parse.map2
                    (\name text ->
                        Review
                            { params
                                | name = name
                                , text = text
                            }
                    )
                    (Parse.field NameId (Parse.maybe Parse.string))
                    (Parse.field HelpId (Parse.maybe Parse.string))
                )
                field

        Help params ->
            Parse.parse
                (Parse.map3
                    (\name text button ->
                        Help
                            { params
                                | name = name
                                , text = text
                                , button = button
                            }
                    )
                    (Parse.field NameId (Parse.maybe Parse.string))
                    (Parse.field HelpId (Parse.maybe Parse.string))
                    (Parse.field LabelId (Parse.maybe Parse.string))
                )
                field

        _ ->
            Ok element


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


parseOptions : Field FieldId -> List ( String, String )
parseOptions field =
    Parse.parse
        (Parse.field OptionsId (Parse.list optionParser))
        field
        |> Result.withDefault []


optionParser : Parse.Parser FieldId ( String, String )
optionParser =
    Parse.map2 (\v l -> ( v, l ))
        (Parse.field OptionValueId (Parse.maybe Parse.string |> Parse.map (Maybe.withDefault "")))
        (Parse.field OptionLabelId (Parse.maybe Parse.string |> Parse.map (Maybe.withDefault "")))
