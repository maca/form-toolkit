module FormToolkit.View exposing
    ( View, fromField, toHtml
    , partial
    , customizeErrors, customizeFields
    , customizeGroups, customizeRepeatableFields, customizeRepeatingFieldTemplates
    , FieldAttributes, FieldType(..), Status(..)
    )

{-|


# View

@docs View, fromField, toHtml
@docs partial


# View customizations

@docs customizeErrors, customizeFields
@docs customizeGroups, customizeRepeatableFields, customizeRepeatingFieldTemplates


# Field attributes

@docs FieldAttributes, FieldType, Status

-}

import FormToolkit.Error exposing (Error)
import FormToolkit.Field as Field exposing (Field(..), Msg)
import FormToolkit.Value exposing (Value(..))
import Html exposing (Attribute, Html)
import Internal.Field
import Internal.Utils
import Internal.View
import RoseTree.Tree as Tree


{-| -}
type FieldType
    = Text
    | TextArea
    | Email
    | Url
    | Password
    | StrictAutocomplete
    | Integer
    | Float
    | Month
    | Date
    | LocalDatetime
    | Select
    | Radio
    | Checkbox
    | File
    | Group
    | Repeatable


{-| -}
type Status
    = Pristine
    | Focused
    | Editing
    | Touched


{-| -}
type alias FieldAttributes id =
    { fieldType : FieldType
    , name : Maybe String
    , value : Value
    , isRequired : Bool
    , label : Maybe String
    , placeholder : Maybe String
    , hint : Maybe String
    , min : Value
    , max : Value
    , step : Value
    , autogrow : Bool
    , options : List ( String, Value )
    , identifier : Maybe id
    , status : Status
    , repeatableMin : Int
    , repeatableMax : Maybe Int
    , addFieldsButtonCopy : String
    , removeFieldsButtonCopy : String
    , errors : List (Error id)
    , classList : List String
    , selectionStart : Int
    , selectionEnd : Int
    , disabled : Bool
    , hidden : Bool
    , pattern : List Internal.Utils.MaskToken
    , acceptedMimeTypes : List String
    }


{-| A view is a way to configure the generated markup for an `Field` or group
of fields.
-}
type View id msg
    = View (Internal.View.View id msg)


{-| Construct a view from an `Field`.

    view : Html (Never -> a)
    view =
        Field.group []
            [ Field.text [ Field.label "First Name" ]
            , Field.text [ Field.label "Last Name" ]
            ]
            |> View.fromField (always never)
            |> View.toHtml

-}
fromField : (Msg id -> msg) -> Field id -> View id msg
fromField toMsg (Field field) =
    View
        (Internal.View.init
            { onChange = \id path value cursorPos -> toMsg (Field.InputChanged id path value cursorPos)
            , onCheck = \id path checked -> toMsg (Field.OnCheck id path checked)
            , onFileSelect = \id path fileValue -> toMsg (Field.FileSelected id path fileValue)
            , onFocus = \id path -> toMsg (Field.InputFocused id path)
            , onBlur = \id path -> toMsg (Field.InputBlured id path)
            , onAdd = \id path -> toMsg (Field.InputsAdded id path)
            , onRemove = \id path -> toMsg (Field.InputsRemoved id path)
            , path = []
            , field = field
            }
        )


{-| Render a view
-}
toHtml : View id msg -> Html msg
toHtml (View view) =
    Internal.View.toHtml view


{-| A partial view referenced by `identifier`.
Maybe you want to render segments of the same form in different UI sections.

    Field.group []
        [ Field.text
            [ Field.identifier "FirstName"
            , Field.label "First Name"
            ]
        , Field.text
            [ Field.identifier "LastName"
            , Field.label "Last Name"
            ]
        ]
        |> View.fromField (always never)
        |> View.partial "FirstName"
        == Just
            (Field.text
                [ Field.identifier "FirstName"
                , Field.label "First Name"
                ]
            )

-}
partial : id -> View id msg -> Maybe (View id msg)
partial id (View view) =
    Internal.View.partial id view |> Maybe.map View


{-| Customizes how the error messages are displayed, to be used for i18n errors.
The function receives field attributes including all errors for the field.
It's possible to override error messages for all fields, individual fields,
or fields of a certain type.

See [FieldAttributes](#FieldAttributes) for all the available field attributes.

    type Fields
        = Name
        | Temperature
        | Flavour

    view : View Fields val ()
    view =
        Field.group []
            [ Field.text
                [ Field.label "Name"
                , Field.identifier Name
                , Field.required
                ]
            , Field.text
                [ Field.label "Temperature"
                , Field.identifier Temperature
                , Field.min 20
                , Field.max 35
                , Field.required
                ]
            , Field.select
                [ Field.label "Flavour"
                , Field.identifier Flavour
                , Field.required
                , Field.options
                    [ ( "Banana", Value.string "banana" )
                    , ( "Strawbery", Value.string "strawberry" )
                    , ( "Chocolate", Value.string "chocolate" )
                    ]
                ]
            ]
            |> View.fromField (always ())
            |> View.customizeErrors
                (\attributes ->
                    let
                        toString =
                            Value.toString >> Maybe.withDefault ""

                        errorToString error =
                            case ( attributes.inputType, error ) of
                                ( _, ValueTooLarge _ data ) ->
                                    toString data.max ++ " is too high"

                                ( _, ValueTooSmall _ data ) ->
                                    toString data.min ++ " is too low"

                                ( _, ValueNotInRange _ data ) ->
                                    "Make it in between " ++ toString data.min ++ " and " ++ toString data.max

                                ( Select, IsBlank _ ) ->
                                    "Make up your mind"

                                ( _, IsBlank (Just Name) ) ->
                                    "Who are you?"

                                ( _, IsBlank _ ) ->
                                    "You forgot to fill in this"

                                ( _, CustomError _ message ) ->
                                    message

                                _ ->
                                    "Humm...?"
                    in
                    attributes.errors
                        |> List.map errorToString
                        |> String.join ", "
                )

-}
customizeErrors : (FieldAttributes id -> String) -> View id msg -> View id msg
customizeErrors viewFunc (View view) =
    View { view | errorToString = \attrs _ -> viewFunc (mapAttributes (Field (Tree.leaf attrs))) }


{-| Provide a function to override the rendering of a field.

The function receives a configuration record with pre-rendered HTML elements
and field attributes, and should return `Just (Html msg)` for custom rendering
or `Nothing` to use the default rendering.

The `labelHtml`, `fieldHtml`, and `hintHtml` functions take a list of `Html.Attribute msg`
to allow customization of the rendered elements.

Use `attributes` to access field properties like `identifier` for pattern
matching on specific fields. See [FieldAttributes](#FieldAttributes) for all the
avaiable field attributes.

The example below shows custom rendering for a specific field identifier:

    view : View id val ()
    view =
        Field.text
            [ Field.label "Name"
            , Field.identifier MyField
            ]
            |> View.fromField (always ())
            |> customizeFields
                (\{ attributes, labelHtml, fieldHtml, hintHtml, errors } ->
                    case attributes.identifier of
                        Just MyField ->
                            -- Custom rendering for MyField
                            Just
                                (Html.div
                                    [ Attributes.class "field" ]
                                    [ labelHtml [ Attributes.class "custom-label" ]
                                    , Html.div
                                        [ Attributes.class "input-wrapper" ]
                                        [ fieldHtml []
                                        , hintHtml []
                                        ]
                                    ]
                                )

                        _ ->
                            -- Use default rendering for other fields
                            Nothing
                )

-}
customizeFields :
    ({ labelHtml : List (Attribute msg) -> Html msg
     , fieldHtml : List (Attribute msg) -> Html msg
     , hintHtml : List (Attribute msg) -> Html msg
     , errors : List String
     , class : String
     , attributes : FieldAttributes id
     , inputOnChange : Value -> { selectionStart : Int, selectionEnd : Int } -> msg
     , inputOnCheck : Bool -> msg
     , inputOnBlur : msg
     , inputOnFocus : msg
     }
     -> Maybe (Html msg)
    )
    -> View id msg
    -> View id msg
customizeFields viewFunc (View view) =
    let
        defaultViewFunction =
            view.fieldView

        fieldView =
            \params ->
                case
                    viewFunc
                        { labelHtml = params.labelHtml
                        , fieldHtml = params.inputHtml
                        , hintHtml = params.hintHtml
                        , errors = params.errors
                        , class = String.join " " params.attributes.classList
                        , attributes = mapAttributes (Field (Tree.leaf params.attributes))
                        , inputOnChange =
                            \(Value val) cursorPos -> view.onChange params.attributes.identifier params.path val cursorPos
                        , inputOnCheck = \checked -> view.onCheck params.attributes.identifier params.path checked
                        , inputOnBlur = view.onBlur params.attributes.identifier params.path
                        , inputOnFocus = view.onFocus params.attributes.identifier params.path
                        }
                of
                    Just html ->
                        html

                    Nothing ->
                        defaultViewFunction params
    in
    View
        { view
            | fieldView = fieldView
            , checkboxFieldView = fieldView
        }


{-| Provide a function to customize the rendering of a group of fields.

    view : View id val ()
    view =
        Field.group []
            [ Field.text [ Field.label "Name" ]
            , Field.text [ Field.label "Last Name" ]
            ]
            |> View.fromField (always ())
            |> customizeField
                (\{ fields, legendText } ->
                    Html.fieldset
                        [ Html.Attribute.class "field-group" ]
                        ((case legendText of
                            Just str ->
                                Html.legend [] [ Html.text str ]

                            Nothing ->
                                Html.text ""
                         )
                            :: fields
                        )
                )

-}
customizeGroups :
    ({ legendText : Maybe String
     , fields : List (Html msg)
     , identifier : Maybe id
     , errors : List String
     , class : String
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
customizeGroups viewFunc (View view) =
    View { view | groupView = viewFunc }


{-| Customize the positioning, and appearance of each of the inputs of a repeatable
group of inputs and the and the button to add new inputs.

The `addFieldsButton` function takes a list of `Html.Attribute msg` to allow
customization of the button element.

To customize the template used to add a new input see
[customizeRepeatingFieldTemplates](#customizeRepeatingFieldTemplates).

    view : View String val ()
    view =
        Field.repeatable [ Field.identifier "People" ]
            (Field.group []
                [ [ Field.text [ Field.label "Name" ]
                  , Field.text [ Field.label "Last Name" ]
                  ]
                ]
            )
            []
            |> View.fromField (always ())
            |> customizeRepeatableFields
                (\{ legendText, fields, addFieldsButton } ->
                    Html.fieldset []
                        [ case legendText of
                            Just str ->
                                Html.legend [] [ Html.text str ]

                            Nothing ->
                                Html.text ""
                        , Html.div [] fields
                        , addFieldsButton []
                        ]
                )

-}
customizeRepeatableFields :
    ({ legendText : Maybe String
     , fields : List (Html msg)
     , addFieldsButton : List (Attribute msg) -> Html msg
     , errors : List String
     , class : String
     , identifier : Maybe id
     , addFieldsButtonOnClick : Maybe msg
     , addFieldsButtonCopy : String
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
customizeRepeatableFields viewFunc (View view) =
    View
        { view
            | repeatableFieldsGroupView =
                \params ->
                    viewFunc
                        { legendText = params.legendText
                        , fields = params.fields
                        , addFieldsButton = params.addFieldsButton
                        , errors = params.errors
                        , class = String.join " " params.attributes.classList
                        , identifier = params.attributes.identifier
                        , addFieldsButtonOnClick = params.addFieldsButtonOnClick
                        , addFieldsButtonCopy = params.attributes.addFieldsButtonCopy
                        }
        }


{-| Customize the rendering of each of the elements of a repeatable group of
inputs.

The `removeFieldsButton` function takes a list of `Html.Attribute msg` to allow
customization of the button element.

To customize the group of inputs see
[customizeRepeatableFields](#customizeRepeatableFields).

    view : View String val ()
    view =
        Field.repeatable [ Field.identifier "People" ]
            (Field.group []
                [ [ Field.text [ Field.label "Name" ]
                  , Field.text [ Field.label "Last Name" ]
                  ]
                ]
            )
            []
            |> View.fromField (always ())
            |> customizeRepeatingFieldTemplates
                (\{ fieldHtml, removeFieldsButton } ->
                    Html.div
                        [ Attributes.class "group-repeat" ]
                        [ fieldHtml, removeFieldsButton [] ]
                )

-}
customizeRepeatingFieldTemplates :
    ({ fieldHtml : Html msg
     , removeFieldsButton : List (Attribute msg) -> Html msg
     , identifier : Maybe id
     , index : Int
     , removeFieldsButtonOnClick : Maybe msg
     , removeFieldsButtonCopy : String
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
customizeRepeatingFieldTemplates viewFunc (View view) =
    View
        { view
            | repeatableFieldView =
                \params ->
                    viewFunc
                        { fieldHtml = params.field
                        , removeFieldsButton = params.removeFieldsButton
                        , identifier = params.attributes.identifier
                        , removeFieldsButtonOnClick = params.removeFieldsButtonOnClick
                        , index = params.index
                        , removeFieldsButtonCopy = params.removeFieldsButtonCopy
                        }
        }


mapAttributes : Field a -> FieldAttributes a
mapAttributes (Field field) =
    field
        |> Tree.mapValues
            (Internal.Field.mapAttributes identity identity mapFieldType Value mapStatus)
        |> Tree.value


mapStatus : Internal.Field.Status -> Status
mapStatus status =
    case status of
        Internal.Field.Pristine ->
            Pristine

        Internal.Field.Focused ->
            Focused

        Internal.Field.Editing ->
            Editing

        Internal.Field.Touched ->
            Touched


mapFieldType : Internal.Field.FieldType id value err -> FieldType
mapFieldType fieldType =
    case fieldType of
        Internal.Field.Repeatable _ ->
            Repeatable

        Internal.Field.Text ->
            Text

        Internal.Field.TextArea ->
            TextArea

        Internal.Field.Email ->
            Email

        Internal.Field.Url ->
            Url

        Internal.Field.Password ->
            Password

        Internal.Field.StrictAutocomplete ->
            StrictAutocomplete

        Internal.Field.Integer ->
            Integer

        Internal.Field.Float ->
            Float

        Internal.Field.Month ->
            Month

        Internal.Field.Date ->
            Date

        Internal.Field.LocalDatetime ->
            LocalDatetime

        Internal.Field.Select ->
            Select

        Internal.Field.Radio ->
            Radio

        Internal.Field.Checkbox ->
            Checkbox

        Internal.Field.File ->
            File

        Internal.Field.Group ->
            Group
