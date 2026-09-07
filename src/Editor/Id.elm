module Editor.Id exposing (Id, fromInt, increment, toIdentifier, unset)

import String.Extra as String


type Id
    = Unset
    | Id Int


unset : Id
unset =
    Unset


fromInt : Int -> Id
fromInt int =
    Id int


increment : Id -> Id
increment id =
    case id of
        Unset ->
            Id 1

        Id int ->
            Id (int + 1)


toInt : Id -> Maybe Int
toInt id =
    case id of
        Unset ->
            Nothing

        Id int ->
            Just int


toIdentifier : Maybe String -> Id -> Maybe String
toIdentifier suffix id =
    toInt id
        |> Maybe.map
            (\i ->
                [ Just (String.fromInt i), suffix ]
                    |> List.filterMap identity
                    |> String.join "-"
                    |> String.dasherize
            )
