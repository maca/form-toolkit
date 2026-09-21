module Internal.Editor.Form exposing
    ( EditorForm, init, isValid, update, view
    , labelInputId, Msg
    )

{-| The property editor for the element selected in the builder. Each editable
property is a `FormToolkit.Field`, and every change is parsed back into an
updated element.


# Editor form

@docs EditorForm, init, isValid, update, view


# Focusing the label

@docs labelInputId, Msg

-}

import FormToolkit.Error exposing (Error)
import FormToolkit.Field as FormField
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value exposing (Value)
import Html exposing (Html)
import Internal.Editor.Element as Element exposing (Element(..))


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


{-| An element's properties, the form holding them, and whether that form has
been edited (`touched`) and still parses (`valid`).
-}
type alias EditorForm =
    { element : Element
    , field : FormField.Field FieldId
    , touched : Bool
    , valid : Bool
    }


type Msg
    = FieldMsg (FormField.Msg FieldId)


init : Element -> EditorForm
init element =
    { element = element
    , field = fromElement element
    , touched = False
    , valid = isValid element
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
                newField =
                    FormField.update fieldMsg form.field

                updatedElement =
                    parseElement form.element newField
            in
            ( { form
                | field = newField
                , element = Result.withDefault form.element updatedElement
                , touched = form.touched || edited fieldMsg
                , valid =
                    case updatedElement of
                        Ok _ ->
                            True

                        Err _ ->
                            False
              }
            , updatedElement
            )


view : EditorForm -> Html Msg
view form =
    FormField.toHtml FieldMsg form.field


{-| Focusing and blurring a field is not an edit: the builder flags invalid
nodes only once their form has been typed in.
-}
edited : FormField.Msg FieldId -> Bool
edited fieldMsg =
    case fieldMsg of
        FormField.InputFocused _ _ ->
            False

        FormField.InputBlured _ _ ->
            False

        _ ->
            True


{-| DOM id of the element's Label input, when its form has one: the `Help` form
labels its `Button Text` input, and `Review` and `Blank` have no label. The
builder focuses it when a node is selected.
-}
labelInputId : Element -> Maybe String
labelInputId element =
    case element of
        Help _ ->
            Just (labelFieldName ++ "-2")

        Review _ ->
            Nothing

        Blank _ ->
            Nothing

        _ ->
            Just (labelFieldName ++ "-1")


{-| `Field.name` is what gives an input its DOM id; a preview rendered next to
the builder would otherwise claim the same type-based ids.
-}
labelFieldName : String
labelFieldName =
    "editor-label"


fromElement : Element -> FormField.Field FieldId
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


fromFieldElement : Element.FieldParams -> FormField.Field FieldId
fromFieldElement params =
    case params.field of
        Element.TextField ->
            textFieldForm params

        Element.Checkbox ->
            checkboxFieldForm params

        Element.IntegerField { min, max } ->
            rangeFieldForm params
                (FormField.text [ FormField.identifier MinId, FormField.label "Min", FormField.value (Value.string (Maybe.withDefault "" (Value.toString min))) ])
                (FormField.text [ FormField.identifier MaxId, FormField.label "Max", FormField.value (Value.string (Maybe.withDefault "" (Value.toString max))) ])

        Element.DateField { min, max } ->
            rangeFieldForm params
                (FormField.text [ FormField.identifier MinId, FormField.label "Min", FormField.value (Value.string (Maybe.withDefault "" (Value.toString min))) ])
                (FormField.text [ FormField.identifier MaxId, FormField.label "Max", FormField.value (Value.string (Maybe.withDefault "" (Value.toString max))) ])

        Element.MonthField { min, max } ->
            rangeFieldForm params
                (FormField.text [ FormField.identifier MinId, FormField.label "Min", FormField.value (Value.string (Maybe.withDefault "" (Value.toString min))) ])
                (FormField.text [ FormField.identifier MaxId, FormField.label "Max", FormField.value (Value.string (Maybe.withDefault "" (Value.toString max))) ])

        Element.Select options ->
            optionsFieldForm params options

        Element.Radio options ->
            optionsFieldForm params options


textFieldForm : Element.FieldParams -> FormField.Field FieldId
textFieldForm params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.nameAttribute params.attributes)))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.name labelFieldName
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.labelAttribute params.attributes)))
            , FormField.required True
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.placeholderAttribute params.attributes)))
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.hintAttribute params.attributes)))
            ]
        , FormField.checkbox
            [ FormField.identifier RequiredId
            , FormField.label "Is Required?"
            , FormField.value (Value.bool (Element.requiredAttribute params.attributes))
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Field Help"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.helpTextAttribute params.attributes)))
            ]
        ]


checkboxFieldForm : Element.FieldParams -> FormField.Field FieldId
checkboxFieldForm params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.nameAttribute params.attributes)))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.name labelFieldName
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.labelAttribute params.attributes)))
            , FormField.required True
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.placeholderAttribute params.attributes)))
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.hintAttribute params.attributes)))
            ]
        , FormField.checkbox
            [ FormField.identifier RequiredId
            , FormField.label "Is Required?"
            , FormField.value (Value.bool (Element.requiredAttribute params.attributes))
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Field Help"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.helpTextAttribute params.attributes)))
            ]
        ]


rangeFieldForm : Element.FieldParams -> FormField.Field FieldId -> FormField.Field FieldId -> FormField.Field FieldId
rangeFieldForm params minField maxField =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.nameAttribute params.attributes)))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.name labelFieldName
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.labelAttribute params.attributes)))
            , FormField.required True
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.placeholderAttribute params.attributes)))
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.hintAttribute params.attributes)))
            ]
        , minField
        , maxField
        , FormField.checkbox
            [ FormField.identifier RequiredId
            , FormField.label "Is Required?"
            , FormField.value (Value.bool (Element.requiredAttribute params.attributes))
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Field Help"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.helpTextAttribute params.attributes)))
            ]
        ]


optionsFieldForm : Element.FieldParams -> List ( String, String ) -> FormField.Field FieldId
optionsFieldForm params options =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.nameAttribute params.attributes)))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.name labelFieldName
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.labelAttribute params.attributes)))
            , FormField.required True
            ]
        , FormField.text
            [ FormField.identifier PlaceholderId
            , FormField.label "Placeholder"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.placeholderAttribute params.attributes)))
            ]
        , FormField.text
            [ FormField.identifier HintId
            , FormField.label "Hint"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.hintAttribute params.attributes)))
            ]
        , FormField.checkbox
            [ FormField.identifier RequiredId
            , FormField.label "Is Required?"
            , FormField.value (Value.bool (Element.requiredAttribute params.attributes))
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
            , FormField.value (Value.string (Maybe.withDefault "" (Element.helpTextAttribute params.attributes)))
            ]
        ]


optionTemplate : FormField.Field FieldId
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


optionInitializer : ( String, String ) -> (FormField.Field FieldId -> FormField.Field FieldId)
optionInitializer ( optionValue, label_ ) =
    \template ->
        template
            |> FormField.updateWithId OptionValueId (FormField.value (Value.string optionValue))
            |> FormField.updateWithId OptionLabelId (FormField.value (Value.string label_))


fromGroupElement : { a | attributes : List Element.Attribute } -> FormField.Field FieldId
fromGroupElement params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.nameAttribute params.attributes)))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.name labelFieldName
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.labelAttribute params.attributes)))
            ]
        , FormField.select
            [ FormField.identifier InlineId
            , FormField.label "Format"
            , FormField.options
                [ ( "Stacked", Value.bool False )
                , ( "Inline", Value.bool True )
                ]
            , FormField.value (Value.bool (Element.inlineAttribute params.attributes))
            , FormField.required True
            ]
        ]


fromRepeatableGroupElement : { a | attributes : List Element.Attribute } -> FormField.Field FieldId
fromRepeatableGroupElement params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.nameAttribute params.attributes)))
            , FormField.hint "Can only contain downcase and `_` characters"
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.name labelFieldName
            , FormField.label "Label"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.labelAttribute params.attributes)))
            ]
        , FormField.select
            [ FormField.identifier InlineId
            , FormField.label "Format"
            , FormField.options
                [ ( "Stacked", Value.bool False )
                , ( "Inline", Value.bool True )
                ]
            , FormField.value (Value.bool (Element.inlineAttribute params.attributes))
            , FormField.required True
            ]
        ]


reviewForm : { a | attributes : List Element.Attribute } -> FormField.Field FieldId
reviewForm params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.nameAttribute params.attributes)))
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Text"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.textAttribute params.attributes)))
            ]
        ]


helpForm : { a | attributes : List Element.Attribute } -> FormField.Field FieldId
helpForm params =
    FormField.group []
        [ FormField.text
            [ FormField.identifier NameId
            , FormField.label "Name"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.nameAttribute params.attributes)))
            ]
        , FormField.textarea
            [ FormField.identifier HelpId
            , FormField.label "Text"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.textAttribute params.attributes)))
            ]
        , FormField.text
            [ FormField.identifier LabelId
            , FormField.name labelFieldName
            , FormField.label "Button Text"
            , FormField.value (Value.string (Maybe.withDefault "" (Element.buttonAttribute params.attributes)))
            ]
        ]


parseElement : Element -> FormField.Field FieldId -> Result (Error FieldId) Element
parseElement element field =
    case element of
        FieldElement params ->
            let
                required =
                    getBoolValue RequiredId field (Element.requiredAttribute params.attributes)

                hint =
                    Parse.parse (Parse.field HintId (Parse.maybe Parse.string)) field
                        |> Result.withDefault (Element.hintAttribute params.attributes)

                help =
                    Parse.parse (Parse.field HelpId (Parse.maybe Parse.string)) field
                        |> Result.withDefault (Element.helpTextAttribute params.attributes)
            in
            Parse.parse
                (Parse.map3
                    (\name label placeholder ->
                        FieldElement
                            { params
                                | attributes =
                                    fieldElementAttributes
                                        { name = name
                                        , label = label
                                        , placeholder = placeholder
                                        , hint = hint
                                        , help = help
                                        , required = required
                                        }
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
                            { params | attributes = groupElementAttributes name label inline }
                    )
                    (Parse.field NameId (Parse.maybe Parse.string))
                    (Parse.field LabelId (Parse.maybe Parse.string))
                    (Parse.maybe (Parse.field InlineId Parse.bool) |> Parse.map (Maybe.withDefault (Element.inlineAttribute params.attributes)))
                )
                field

        RepeatableGroup params ->
            Parse.parse
                (Parse.map3
                    (\name label inline ->
                        RepeatableGroup
                            { params | attributes = groupElementAttributes name label inline }
                    )
                    (Parse.field NameId (Parse.maybe Parse.string))
                    (Parse.field LabelId (Parse.maybe Parse.string))
                    (Parse.maybe (Parse.field InlineId Parse.bool) |> Parse.map (Maybe.withDefault (Element.inlineAttribute params.attributes)))
                )
                field

        Review params ->
            Parse.parse
                (Parse.map2
                    (\name text ->
                        Review
                            { params | attributes = reviewElementAttributes name text }
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
                            { params | attributes = helpElementAttributes name button text }
                    )
                    (Parse.field NameId (Parse.maybe Parse.string))
                    (Parse.field HelpId (Parse.maybe Parse.string))
                    (Parse.field LabelId (Parse.maybe Parse.string))
                )
                field

        _ ->
            Ok element


{-| The attribute list the field property editor produces.
-}
fieldElementAttributes :
    { name : Maybe String
    , label : Maybe String
    , placeholder : Maybe String
    , hint : Maybe String
    , help : Maybe String
    , required : Bool
    }
    -> List Element.Attribute
fieldElementAttributes props =
    [ Maybe.map Element.Name props.name
    , Maybe.map Element.Label props.label
    , Maybe.map Element.Placeholder props.placeholder
    , Maybe.map Element.Hint props.hint
    , Maybe.map Element.HelpText props.help
    , Just (Element.Required props.required)
    ]
        |> List.filterMap identity


{-| The attribute list the group property editor produces.
-}
groupElementAttributes : Maybe String -> Maybe String -> Bool -> List Element.Attribute
groupElementAttributes name label inline =
    [ Maybe.map Element.Name name
    , Maybe.map Element.Label label
    , Just (Element.Inline inline)
    ]
        |> List.filterMap identity


reviewElementAttributes : Maybe String -> Maybe String -> List Element.Attribute
reviewElementAttributes name text =
    [ Maybe.map Element.Name name
    , Maybe.map Element.Text text
    ]
        |> List.filterMap identity


helpElementAttributes : Maybe String -> Maybe String -> Maybe String -> List Element.Attribute
helpElementAttributes name button text =
    [ Maybe.map Element.Name name
    , Maybe.map Element.Button button
    , Maybe.map Element.Text text
    ]
        |> List.filterMap identity


parseFieldType : Element.Field -> FormField.Field FieldId -> Element.Field
parseFieldType fieldType field =
    case fieldType of
        Element.IntegerField _ ->
            Element.IntegerField
                { min = getValueByIdWithDefault MinId field Value.blank
                , max = getValueByIdWithDefault MaxId field Value.blank
                }

        Element.DateField _ ->
            Element.DateField
                { min = getValueByIdWithDefault MinId field Value.blank
                , max = getValueByIdWithDefault MaxId field Value.blank
                }

        Element.MonthField _ ->
            Element.MonthField
                { min = getValueByIdWithDefault MinId field Value.blank
                , max = getValueByIdWithDefault MaxId field Value.blank
                }

        Element.Select _ ->
            Element.Select (parseOptions field)

        Element.Radio _ ->
            Element.Radio (parseOptions field)

        _ ->
            fieldType


getBoolValue : FieldId -> FormField.Field FieldId -> Bool -> Bool
getBoolValue id field default =
    Parse.parse (Parse.field id (Parse.maybe Parse.bool)) field
        |> Result.toMaybe
        |> Maybe.andThen identity
        |> Maybe.withDefault default


getValueByIdWithDefault : FieldId -> FormField.Field FieldId -> Value -> Value
getValueByIdWithDefault id field default =
    Parse.parse (Parse.field id (Parse.maybe Parse.value)) field
        |> Result.toMaybe
        |> Maybe.andThen identity
        |> Maybe.withDefault default


parseOptions : FormField.Field FieldId -> List ( String, String )
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
