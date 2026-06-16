module Editor.Form exposing (fromElement, updateElement)

import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)
import Editor.Element as Editor
    exposing
        ( Element(..)
        , Field(..)
        , FieldParams
        , Options
        )
import FormToolkit.Form as Form exposing (Form)
import FormToolkit.Input as Input
    exposing
        ( Input
        , hint
        , inline
        , label
        , name
        , options
        , reject
        , required
        , value
        )
import FormToolkit.Value as Value exposing (Value, blank, boolean, string)
import Locale exposing (Locale(..))
import String.Extra as String


fromElement : Locale -> List String -> Editor.Element -> Form
fromElement locale fieldNames element =
    case element of
        ElementGroup params ->
            fromGroup locale fieldNames params

        RepeatableGroup params ->
            fromRepeatableGroup locale params

        FieldElement params ->
            fromField locale params

        Review params ->
            fromReview locale fieldNames params

        Help params ->
            fromHelp locale params

        Blank _ ->
            Form.init locale False []


updateElement : Locale -> Form -> Editor.Element -> Editor.Element
updateElement locale form element =
    let
        values =
            Form.toValues form
    in
    case element of
        ElementGroup params ->
            ElementGroup
                { params
                    | name = getValue Value.toString params.name "name" values
                    , label = updateCopies locale "label" values params.label
                    , inline =
                        getValue Value.toBool params.inline "inline" values
                }

        RepeatableGroup params ->
            RepeatableGroup
                { params
                    | name = getValue Value.toString params.name "name" values
                    , label = updateCopies locale "label" values params.label
                    , inline =
                        getValue Value.toBool params.inline "inline" values
                }

        FieldElement params ->
            FieldElement
                { params
                    | name = getValue Value.toString params.name "name" values
                    , label = updateCopies locale "label" values params.label
                    , placeholder =
                        params.placeholder
                            |> updateCopies locale "placeholder" values
                    , hint = updateCopies locale "hint" values params.hint
                    , help = updateCopies locale "help" values params.help
                    , isRequired =
                        getValue Value.toBool params.isRequired "required" values
                    , field = updateField locale values params.field
                }

        Review params ->
            Review
                { params
                    | name = getValue Value.toString params.name "name" values
                    , text = updateCopies locale "text" values params.text
                }

        Help params ->
            Help
                { params
                    | name = getValue Value.toString params.name "name" values
                    , button = updateCopies locale "button" values params.button
                    , text = updateCopies locale "text" values params.text
                }

        Blank _ ->
            element


updateField : Locale -> Dict String Value -> Field -> Field
updateField locale values field =
    case field of
        IntegerField { min, max } ->
            IntegerField
                { min = valueWithDefault min "min" values
                , max = valueWithDefault max "max" values
                }

        DateField { min, max } ->
            DateField
                { min = valueWithDefault min "min" values
                , max = valueWithDefault max "max" values
                }

        MonthField { min, max } ->
            MonthField
                { min = valueWithDefault min "min" values
                , max = valueWithDefault max "max" values
                }

        Select options ->
            Select (updateOptions locale values options)

        Radio options ->
            Radio (updateOptions locale values options)

        _ ->
            field


getValue : (value -> Maybe a) -> a -> String -> Dict String value -> a
getValue convert default key values =
    Dict.get key values
        |> Maybe.andThen convert
        |> Maybe.withDefault default


valueWithDefault : Value -> String -> Dict String Value -> Value
valueWithDefault default key values =
    Dict.get key values
        |> Maybe.withDefault default


updateCopies :
    Locale
    -> String
    -> Dict String Value
    -> Dict String String
    -> Dict String String
updateCopies =
    updateLocalized Value.toString


updateLocalized :
    (value -> Maybe a)
    -> Locale
    -> String
    -> Dict String value
    -> Dict String a
    -> Dict String a
updateLocalized convert (Locale locale) key values dict =
    case Maybe.andThen convert (Dict.get key values) of
        Just localized ->
            Dict.insert locale localized dict

        Nothing ->
            Dict.remove locale dict


updateOptions : Locale -> Dict String Value -> Options -> Options
updateOptions (Locale locale) values options =
    Dict.get "options" values
        |> Maybe.map Value.toList
        |> Maybe.withDefault []
        |> List.filterMap optionToTuple
        |> List.foldr
            (\( key, val ) acc ->
                ( key
                , case Dict.get key (Dict.fromList options) of
                    Just dict ->
                        Maybe.map (\v -> Dict.insert locale v dict) val
                            |> Maybe.withDefault dict

                    Nothing ->
                        Dict.empty
                )
                    :: acc
            )
            []


optionToTuple : List ( String, Value ) -> Maybe ( String, Maybe String )
optionToTuple list =
    Dict.get "value" (Dict.fromList list)
        |> Maybe.andThen Value.toString
        |> Maybe.map
            (\value ->
                ( value
                , Dict.get "label" (Dict.fromList list)
                    |> Maybe.andThen Value.toString
                )
            )


fromGroup :
    Locale
    -> List String
    -> { a | name : String, label : Dict String String, inline : Bool }
    -> Form
fromGroup locale fieldNames params =
    Form.init locale
        False
        [ nameInput params.name
        , Input.text
            [ name "label"
            , labelWithLocale locale "Label"
            , hint (interpolationInstructions fieldNames)
            , value (localizeValue locale params.label)
            ]
        , Input.select
            [ name "inline"
            , label "Format"
            , options
                [ ( "Stacked", boolean False )
                , ( "Inline", boolean True )
                ]
            , value (boolean params.inline)
            , required True
            ]
        ]


fromRepeatableGroup :
    Locale
    -> { a | name : String, label : Dict String String, inline : Bool }
    -> Form
fromRepeatableGroup locale params =
    Form.init locale
        False
        [ nameInput params.name
        , labelInput locale params.label
        , Input.select
            [ name "inline"
            , label "Format"
            , options
                [ ( "Stacked", boolean False )
                , ( "Inline", boolean True )
                ]
            , value (boolean params.inline)
            , required True
            ]
        ]


fromField : Locale -> FieldParams -> Form
fromField locale params =
    case params.field of
        TextField ->
            inputForm locale params

        IntegerField range ->
            rangeInputForm locale Input.integer params range

        DateField range ->
            rangeInputForm locale Input.date params range

        MonthField range ->
            rangeInputForm locale Input.month params range

        Select options ->
            optionsForm locale options params

        Radio options ->
            optionsForm locale options params

        Checkbox ->
            Form.init locale
                False
                [ nameInput params.name
                , fieldLabelInput locale params.label
                , placeholderInput locale params.placeholder
                , hintInput locale params.hint
                , helpInput locale params.help
                ]


inputForm : Locale -> FieldParams -> Form
inputForm locale params =
    Form.init locale
        False
        [ nameInput params.name
        , fieldLabelInput locale params.label
        , placeholderInput locale params.placeholder
        , hintInput locale params.hint
        , isRequiredCheckbox params.isRequired
        , helpInput locale params.help
        ]


rangeInputForm :
    Locale
    -> (List Input.Attribute -> Input)
    -> FieldParams
    -> { min : Value, max : Value }
    -> Form
rangeInputForm locale input params range =
    Form.init locale
        False
        [ nameInput params.name
        , fieldLabelInput locale params.label
        , placeholderInput locale params.placeholder
        , hintInput locale params.hint
        , rangeFields input range
        , isRequiredCheckbox params.isRequired
        , helpInput locale params.help
        ]


fromReview :
    Locale
    -> List String
    -> { a | name : String, text : Dict String String }
    -> Form
fromReview locale fieldNames params =
    Form.init locale
        False
        [ nameInput params.name
        , Input.textarea
            [ name "text"
            , labelWithLocale locale "Text"
            , hint (interpolationWithMarkdownInstructions fieldNames)
            , required True
            , value (localizeValue locale params.text)
            ]
        ]


fromHelp :
    Locale
    ->
        { a
            | name : String
            , button : Dict String String
            , text : Dict String String
        }
    -> Form
fromHelp locale params =
    Form.init locale
        False
        [ nameInput params.name
        , Input.textarea
            [ name "text"
            , labelWithLocale locale "Text"
            , required True
            , value (localizeValue locale params.text)
            , hint markdownInstructions
            ]
        , Input.text
            [ name "button"
            , label "Button Text"
            , required True
            , value (localizeValue locale params.button)
            ]
        ]


optionsForm :
    Locale
    -> List ( String, Dict String String )
    -> FieldParams
    -> Form
optionsForm locale options params =
    Form.init locale
        False
        [ nameInput params.name
        , fieldLabelInput locale params.label
        , placeholderInput locale params.placeholder
        , hintInput locale params.hint
        , isRequiredCheckbox params.isRequired
        , optionsInput locale options
        , helpInput locale params.help
        ]


optionsInput : Locale -> List ( String, Dict String String ) -> Input
optionsInput locale options =
    Input.repeatable
        (optionInput locale "" Dict.empty)
        [ name "options", label "Options" ]
        (List.map (uncurry (optionInput locale)) options)


optionInput : Locale -> String -> Dict String String -> Input
optionInput locale optionValue labels =
    Input.group
        [ inline True ]
        [ Input.text
            [ name "value"
            , label "Option Value"
            , required True
            , value (string optionValue)
            ]
        , Input.text
            [ name "label"
            , labelWithLocale locale "Option Label"
            , required True
            , value (localizeValue locale labels)
            ]
        ]


nameInput : String -> Input
nameInput inputName =
    Input.text
        [ name "name"
        , label "Name"
        , value (string inputName)
        , hint "Can only contain downcase and `_` characters"
        , required True
        , reject "[^a-z_]"
        ]


labelInput : Locale -> Dict String String -> Input
labelInput locale labels =
    Input.text
        [ name "label"
        , labelWithLocale locale "Label"
        , value (localizeValue locale labels)
        ]


fieldLabelInput : Locale -> Dict String String -> Input
fieldLabelInput locale labels =
    Input.text
        [ name "label"
        , labelWithLocale locale "Label"
        , value (localizeValue locale labels)
        , hint "If not provided **Field Name** will be used"
        ]


placeholderInput : Locale -> Dict String String -> Input
placeholderInput locale placeholders =
    Input.text
        [ name "placeholder"
        , labelWithLocale locale "Placeholder"
        , value (localizeValue locale placeholders)
        ]


hintInput : Locale -> Dict String String -> Input
hintInput locale placeholders =
    Input.text
        [ name "hint"
        , labelWithLocale locale "Hint"
        , value (localizeValue locale placeholders)
        , hint markdownInstructions
        ]


isRequiredCheckbox : Bool -> Input
isRequiredCheckbox isRequired =
    Input.checkbox
        [ name "required"
        , label "Is Required?"
        , value (boolean isRequired)
        ]


helpInput : Locale -> Dict String String -> Input
helpInput locale help =
    Input.textarea
        [ name "help"
        , labelWithLocale locale "Field Help"
        , hint markdownInstructions
        , value (localizeValue locale help)
        ]


rangeFields :
    (List Input.Attribute -> Input)
    -> { min : Value, max : Value }
    -> Input
rangeFields input { min, max } =
    Input.group
        [ inline True ]
        [ input
            [ name "min"
            , label "Minimum Value"
            , value min
            ]
        , input
            [ name "max"
            , label "Maximum Value"
            , value max
            ]
        ]


markdownInstructions : String
markdownInstructions =
    """
Markdown is used for basic formatting: \\*\\***bold**\\*\\*,
\\__italic_\\_, \\_\\*\\*_**italic bold**_\\*\\*\\_, `*` and `_` are
interchangeable.

Formatting allows for headers, numbered lists, lists and links.  For more
information check this markdown
[guide](https://www.markdownguide.org/cheat-sheet/).
    """


availableFieldInstructions : List String -> String
availableFieldInstructions fieldNames =
    """
A field name surrounded by `{{` and `}}` will be replaced by the value of the
field.

Available fields:"""
        ++ String.toSentence (List.map (String.surround "`") fieldNames)
        ++ "."
        ++ """
        """


interpolationInstructions : List String -> String
interpolationInstructions fieldNames =
    availableFieldInstructions fieldNames
        ++ """

For example: `{{name}} was born in {{birth_date}}` will be displayed as
"Frank was born in 1900"."""


interpolationWithMarkdownInstructions : List String -> String
interpolationWithMarkdownInstructions fieldNames =
    availableFieldInstructions fieldNames
        ++ """

For example: `**{{name}}** was born in {{birth_date}}` will be displayed as
"**Frank** was born in 1900".

"""
        ++ markdownInstructions



-- HELPERS


localizeValue : Locale -> Dict String String -> Value
localizeValue (Locale locale) dict =
    Dict.get locale dict
        |> Maybe.map string
        |> Maybe.withDefault blank


labelWithLocale : Locale -> String -> Input.Attribute
labelWithLocale (Locale locale) string =
    label (string ++ " (" ++ String.toUpper locale ++ ")")
