module FormToolkit.Error exposing (Error(..), toEnglish, toList)

{-|

@docs Error, toEnglish, toList

-}

import FormToolkit.Value as Value


{-| Represents an error that occurred during decoding or validation.
-}
type Error id
    = IsBlank (Maybe id)
    | ValueTooLarge (Maybe id) { value : Value.Value, max : Value.Value }
    | ValueTooSmall (Maybe id) { value : Value.Value, min : Value.Value }
    | ValueNotInRange
        (Maybe id)
        { value : Value.Value
        , min : Value.Value
        , max : Value.Value
        }
    | NotNumber (Maybe id)
    | NotBool (Maybe id)
    | HasNoName (Maybe id)
    | PatternError (Maybe id)
    | EmailInvalid (Maybe id)
    | UrlInvalid (Maybe id)
    | MimeTypeInvalid (Maybe id) { mime : String, accepted : List String }
    | IsGroupNotInput (Maybe id)
    | NoOptionsProvided (Maybe id)
    | InvalidValue (Maybe id)
    | InputNotFound id
    | ErrorList (Maybe id) (List (Error id))
    | ParseError (Maybe id)
    | CustomError (Maybe id) String


{-| -}
toEnglish : Error id -> String
toEnglish error =
    let
        toString =
            Value.toString >> Maybe.withDefault ""
    in
    case error of
        IsBlank _ ->
            "Should be provided"

        ValueTooLarge _ data ->
            "Should be lesser than " ++ toString data.max

        ValueTooSmall _ data ->
            "Should be greater than " ++ toString data.min

        ValueNotInRange _ data ->
            "Should be between " ++ toString data.min ++ " and " ++ toString data.max

        NotNumber _ ->
            "Must be a number"

        NotBool _ ->
            "Must be true or false"

        HasNoName _ ->
            "Couldn't parse"

        PatternError _ ->
            "Doesn't match the required pattern"

        EmailInvalid _ ->
            "Please enter a valid email address"

        UrlInvalid _ ->
            "Please enter a valid URL"

        MimeTypeInvalid _ data ->
            "File type '" ++ data.mime ++ "' is not accepted. Accepted types: " ++ String.join ", " data.accepted

        IsGroupNotInput _ ->
            "A group cannot have a value but the decoder is attempting to read the value"

        NoOptionsProvided _ ->
            "No options have been provided"

        InvalidValue _ ->
            "This is not an allowed option"

        InputNotFound _ ->
            "Couldn't find an input with the given identifier"

        ErrorList _ errors ->
            "All of the following failed: " ++ String.join ", " (List.map toEnglish errors)

        ParseError _ ->
            "Couldn't parse"

        CustomError _ message ->
            message


{-| -}
toList : Error id -> List (Error id)
toList error =
    case error of
        ErrorList _ errs ->
            List.concatMap toList errs

        _ ->
            [ error ]
