module Internal.Field exposing (Attributes, FieldType(..), Status(..), mapAttributes, inputStringToValue)

{-|

@docs Attributes, FieldType, Status, mapAttributes, inputStringToValue

-}

import Array
import Dict
import Internal.Utils
import Internal.Value exposing (Value)
import RoseTree.Tree as Tree


type Status
    = Pristine
    | Focused
    | Editing
    | Touched


type FieldType id value err
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
    | Repeatable (Tree.Tree (Attributes id (FieldType id value err) value Status err))


type alias Attributes id fieldType value status err =
    { fieldType : fieldType
    , name : Maybe String
    , value : value
    , isRequired : Bool
    , label : Maybe String
    , placeholder : Maybe String
    , hint : Maybe String
    , min : value
    , max : value
    , step : value
    , autogrow : Bool
    , options : List ( String, value )
    , identifier : Maybe id
    , status : status
    , repeatableMin : Int
    , repeatableMax : Maybe Int
    , addFieldsButtonCopy : String
    , removeFieldsButtonCopy : String
    , errors : err
    , classList : List String
    , selectionStart : Int
    , selectionEnd : Int
    , disabled : Bool
    , hidden : Bool
    , pattern : List Internal.Utils.MaskToken
    , acceptedMimeTypes : List String
    }


mapAttributes :
    (a -> b)
    -> (err1 -> err2)
    -> (type1 -> type2)
    -> (value1 -> value2)
    -> (status1 -> status2)
    -> Attributes a type1 value1 status1 err1
    -> Attributes b type2 value2 status2 err2
mapAttributes func errMapper typeMapper valueMapper statusMapper input =
    { fieldType = typeMapper input.fieldType
    , name = input.name
    , value = valueMapper input.value
    , isRequired = input.isRequired
    , label = input.label
    , placeholder = input.placeholder
    , hint = input.hint
    , min = valueMapper input.min
    , max = valueMapper input.max
    , step = valueMapper input.step
    , autogrow = input.autogrow
    , options = List.map (\( str, val ) -> ( str, valueMapper val )) input.options
    , identifier = Maybe.map func input.identifier
    , status = statusMapper input.status
    , repeatableMin = input.repeatableMin
    , repeatableMax = input.repeatableMax
    , addFieldsButtonCopy = input.addFieldsButtonCopy
    , removeFieldsButtonCopy = input.removeFieldsButtonCopy
    , errors = errMapper input.errors
    , classList = input.classList
    , selectionStart = input.selectionStart
    , selectionEnd = input.selectionEnd
    , disabled = input.disabled
    , hidden = input.hidden
    , pattern = input.pattern
    , acceptedMimeTypes = input.acceptedMimeTypes
    }


type alias Node err id =
    Tree.Tree
        (Attributes id (FieldType id Internal.Value.Value err) Internal.Value.Value Status err)


inputStringToValue : Node err id -> String -> Value
inputStringToValue input str =
    let
        attrs =
            Tree.value input

        getChoice () =
            case String.toInt str of
                Just idx ->
                    Array.fromList attrs.options
                        |> Array.get idx
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault Internal.Value.blank

                Nothing ->
                    Internal.Value.blank
    in
    case attrs.fieldType of
        Text ->
            Internal.Value.fromNonBlankString str

        TextArea ->
            Internal.Value.fromNonEmptyString str

        Password ->
            Internal.Value.fromNonBlankString str

        StrictAutocomplete ->
            if Dict.fromList attrs.options |> Dict.member str then
                Internal.Value.fromNonBlankString str

            else
                Internal.Value.provisional str

        Email ->
            Internal.Value.fromNonBlankString str

        Url ->
            Internal.Value.fromNonBlankString str

        Integer ->
            Internal.Value.intFromString str

        Float ->
            Internal.Value.floatFromString str

        Month ->
            Internal.Value.monthFromString str

        Date ->
            Internal.Value.dateFromString str

        LocalDatetime ->
            Internal.Value.timeFromString str

        Select ->
            getChoice ()

        Radio ->
            getChoice ()

        Checkbox ->
            case str of
                "true" ->
                    Internal.Value.fromBool True

                "false" ->
                    Internal.Value.fromBool False

                _ ->
                    Internal.Value.blank

        File ->
            Internal.Value.blank

        Group ->
            Internal.Value.blank

        Repeatable _ ->
            Internal.Value.blank
