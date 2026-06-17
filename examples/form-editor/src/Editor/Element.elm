module Editor.Element exposing
    ( Element(..)
    , Field(..)
    , FieldParams
    , Options
    , checkbox
    , concatMap
    , date
    , drag
    , elementType
    , elements
    , foldl
    , group
    , groupPlaceholder
    , help
    , icon
    , id
    , identifier
    , integer
    , isEmptyGroup
    , isGroup
    , isPlaceholder
    , label
    , map
    , month
    , name
    , open
    , placeholder
    , prepend
    , radio
    , repeatableGroup
    , review
    , root
    , select
    , text
    , toggleOpen
    , updateDrag
    , updateIds
    , updateIdsFoldFn
    , withId
    )

import Basics.Extra exposing (flip)
import Editor.Drag as Drag exposing (Drag, Position(..))
import Editor.Id as Id exposing (Id)
import FormToolkit.Value as Value exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Schema exposing (GroupParams)
import String.Extra as String


type Field
    = TextField
    | Checkbox
    | IntegerField { min : Value, max : Value }
    | DateField { min : Value, max : Value }
    | MonthField { min : Value, max : Value }
    | Select Options
    | Radio Options


type alias Options =
    List ( String, String )


type alias FieldParams =
    { name : Maybe String
    , label : Maybe String
    , placeholder : Maybe String
    , hint : Maybe String
    , help : Maybe String
    , isRequired : Bool
    , field : Field
    , id : Id
    , drag : Drag
    }


type Element
    = FieldElement FieldParams
    | Review
        { name : Maybe String
        , text : Maybe String
        , id : Id
        , drag : Drag
        }
    | Help
        { name : Maybe String
        , button : Maybe String
        , text : Maybe String
        , id : Id
        , drag : Drag
        }
    | RepeatableGroup
        { name : Maybe String
        , label : Maybe String
        , inline : Bool
        , elements : List Element
        , id : Id
        , drag : Drag
        , isOpen : Bool
        }
    | ElementGroup
        { name : Maybe String
        , label : Maybe String
        , inline : Bool
        , elements : List Element
        , id : Id
        , drag : Drag
        , isOpen : Bool
        }
    | Blank Id


text : Element
text =
    makeField TextField


checkbox : Element
checkbox =
    makeField Checkbox


integer : Element
integer =
    makeField (IntegerField blankRange)


date : Element
date =
    makeField (DateField blankRange)


month : Element
month =
    makeField (MonthField blankRange)


select : Options -> Element
select options =
    makeField (Select options)


radio : Options -> Element
radio options =
    makeField (Radio options)


group : Element
group =
    ElementGroup
        { id = Id.unset
        , name = Nothing
        , label = Nothing
        , inline = False
        , elements = []
        , isOpen = False
        , drag = Drag.idle
        }


repeatableGroup : Element
repeatableGroup =
    RepeatableGroup
        { id = Id.unset
        , name = Nothing
        , label = Nothing
        , inline = False
        , elements = []
        , drag = Drag.idle
        , isOpen = True
        }


review : Element
review =
    Review
        { id = Id.unset
        , name = Nothing
        , text = Nothing
        , drag = Drag.idle
        }


help : Element
help =
    Help
        { id = Id.unset
        , name = Nothing
        , button = Nothing
        , text = Nothing
        , drag = Drag.idle
        }


root : List Element -> Element
root children =
    ElementGroup
        { id = Id.unset
        , name = Just "root"
        , label = Nothing
        , inline = False
        , elements = children
        , drag = Drag.idle
        , isOpen = False
        }


placeholder : Element
placeholder =
    Blank (Id.fromInt -1)


groupPlaceholder : Element
groupPlaceholder =
    Blank (Id.fromInt -2)


blankRange : { min : Value, max : Value }
blankRange =
    { min = Value.blank, max = Value.blank }



-- PARAMS


withId : Id -> Element -> Element
withId elementId element =
    case element of
        ElementGroup params ->
            ElementGroup { params | id = elementId }

        RepeatableGroup params ->
            RepeatableGroup { params | id = elementId }

        FieldElement params ->
            FieldElement { params | id = elementId }

        Review params ->
            Review { params | id = elementId }

        Help params ->
            Help { params | id = elementId }

        Blank _ ->
            Blank elementId


id : Element -> Id
id element =
    case element of
        ElementGroup params ->
            params.id

        RepeatableGroup params ->
            params.id

        FieldElement params ->
            params.id

        Review params ->
            params.id

        Help params ->
            params.id

        Blank id_ ->
            id_


identifier : Element -> Maybe String
identifier element =
    Id.toIdentifier (Just (name element)) (id element)


name : Element -> String
name element =
    case element of
        ElementGroup params ->
            params.name |> Maybe.withDefault ""

        RepeatableGroup params ->
            params.name |> Maybe.withDefault ""

        FieldElement params ->
            params.name |> Maybe.withDefault ""

        Review params ->
            params.name |> Maybe.withDefault ""

        Help params ->
            params.name |> Maybe.withDefault ""

        Blank _ ->
            ""


label : Element -> String
label element =
    case element of
        ElementGroup params ->
            params.label
                |> Maybe.withDefault "Group"

        RepeatableGroup params ->
            params.label
                |> Maybe.withDefault "Repeatable"

        FieldElement params ->
            params.label
                |> Maybe.withDefault (String.toSentenceCase (elementType element))

        Review params ->
            params.name
                |> Maybe.withDefault "Review"

        Help params ->
            params.button
                |> Maybe.withDefault "Help"

        Blank _ ->
            ""


elements : Element -> List Element
elements element =
    case element of
        ElementGroup params ->
            params.elements

        RepeatableGroup params ->
            case params.elements of
                _ :: es ->
                    es

                [] ->
                    []

        _ ->
            []


isEmptyGroup : Element -> Bool
isEmptyGroup element =
    case element of
        ElementGroup params ->
            params.elements == []

        RepeatableGroup params ->
            params.elements == []

        _ ->
            False


isGroup : Element -> Bool
isGroup element =
    case element of
        ElementGroup _ ->
            True

        _ ->
            False


icon : Element -> String
icon element =
    case element of
        ElementGroup _ ->
            "gg-extension-alt"

        RepeatableGroup _ ->
            "gg-extension-add"

        FieldElement params ->
            case params.field of
                TextField ->
                    "gg-edit-flip-h"

                Checkbox ->
                    "gg-check-r"

                IntegerField _ ->
                    "gg-hashtag"

                DateField _ ->
                    "gg-calendar-due"

                MonthField _ ->
                    "gg-calendar-dates"

                Select _ ->
                    "gg-format-separator"

                Radio _ ->
                    "gg-radio-checked"

        Review _ ->
            "gg-flag-alt"

        Help _ ->
            "gg-bulb"

        Blank _ ->
            ""


elementType : Element -> String
elementType element =
    case element of
        ElementGroup _ ->
            "group"

        RepeatableGroup _ ->
            "repeatable-group"

        FieldElement params ->
            case params.field of
                TextField ->
                    "text"

                Checkbox ->
                    "checkbox"

                IntegerField _ ->
                    "integer"

                DateField _ ->
                    "date"

                MonthField _ ->
                    "month"

                Radio _ ->
                    "radio"

                Select _ ->
                    "select"

        Review _ ->
            "review"

        Help _ ->
            "help"

        Blank _ ->
            "placeholder"



-- TREE


drag : Element -> Drag
drag element =
    case element of
        ElementGroup params ->
            params.drag

        RepeatableGroup params ->
            params.drag

        FieldElement params ->
            params.drag

        Review params ->
            params.drag

        Help params ->
            params.drag

        Blank _ ->
            Drag.idle


toggleOpen : Element -> Element
toggleOpen element =
    case element of
        ElementGroup params ->
            ElementGroup { params | isOpen = not params.isOpen }

        RepeatableGroup params ->
            RepeatableGroup
                { params | elements = List.map toggleOpen params.elements }

        _ ->
            element


open : Bool -> Element -> Element
open isOpen element =
    case element of
        ElementGroup params ->
            ElementGroup { params | isOpen = isOpen }

        _ ->
            element


updateDrag : Drag -> Element -> Element
updateDrag dragState element =
    case element of
        ElementGroup params ->
            ElementGroup { params | drag = dragState }

        RepeatableGroup params ->
            RepeatableGroup { params | drag = dragState }

        FieldElement params ->
            FieldElement { params | drag = dragState }

        Review params ->
            Review { params | drag = dragState }

        Help params ->
            Help { params | drag = dragState }

        Blank _ ->
            element


isPlaceholder : Element -> Bool
isPlaceholder element =
    case element of
        Blank _ ->
            True

        _ ->
            False



-- CODEC


decode : Element -> Encode.Value -> Result Decode.Error Element
decode element _ =
    Ok (open True element)


decoder : Decoder Element
decoder =
    Decode.succeed (root [])


encode : Element -> Maybe Encode.Value
encode element =
    let
        maybeValue =
            Encode.object >> Just
    in
    case element of
        ElementGroup attrs ->
            maybeValue
                [ ( "type", Encode.string (elementType element) )
                , ( "inline", Encode.bool attrs.inline )
                , ( "name", encodeMaybeString attrs.name )
                , ( "label", encodeMaybeString attrs.label )
                , ( "fields"
                  , Encode.list identity (List.filterMap encode attrs.elements)
                  )
                ]

        RepeatableGroup attrs ->
            maybeValue
                [ ( "type", Encode.string (elementType element) )
                , ( "inline", Encode.bool attrs.inline )
                , ( "name", encodeMaybeString attrs.name )
                , ( "label", encodeMaybeString attrs.label )
                , ( "fields"
                  , Encode.list identity (List.filterMap encode attrs.elements)
                  )
                ]

        FieldElement attrs ->
            let
                attributesValues =
                    [ ( "type", Encode.string (elementType element) )
                    , ( "name", encodeMaybeString attrs.name )
                    , ( "label", encodeMaybeString attrs.label )
                    , ( "placeholder", encodeMaybeString attrs.placeholder )
                    , ( "help", encodeMaybeString attrs.help )
                    , ( "hint", encodeMaybeString attrs.help )
                    , ( "required", Encode.bool attrs.isRequired )
                    ]
            in
            case attrs.field of
                TextField ->
                    maybeValue attributesValues

                Checkbox ->
                    maybeValue attributesValues

                IntegerField { min, max } ->
                    ( "min", Encode.string (Maybe.withDefault "" (Value.toString min)) )
                        :: ( "max", Encode.string (Maybe.withDefault "" (Value.toString max)) )
                        :: attributesValues
                        |> maybeValue

                DateField { min, max } ->
                    ( "min", Encode.string (Maybe.withDefault "" (Value.toString min)) )
                        :: ( "max", Encode.string (Maybe.withDefault "" (Value.toString max)) )
                        :: attributesValues
                        |> maybeValue

                MonthField { min, max } ->
                    ( "min", Encode.string (Maybe.withDefault "" (Value.toString min)) )
                        :: ( "max", Encode.string (Maybe.withDefault "" (Value.toString max)) )
                        :: attributesValues
                        |> maybeValue

                Select options ->
                    ( "options", encodeOptions options )
                        :: attributesValues
                        |> maybeValue

                Radio options ->
                    ( "options", encodeOptions options )
                        :: attributesValues
                        |> maybeValue

        Review attrs ->
            maybeValue
                [ ( "type", Encode.string (elementType element) )
                , ( "name", encodeMaybeString attrs.name )
                , ( "text", encodeMaybeString attrs.text )
                ]

        Help attrs ->
            maybeValue
                [ ( "type", Encode.string (elementType element) )
                , ( "name", encodeMaybeString attrs.name )
                , ( "button", encodeMaybeString attrs.button )
                , ( "text", encodeMaybeString attrs.text )
                ]

        Blank _ ->
            Nothing


encodeMaybeString : Maybe String -> Encode.Value
encodeMaybeString =
    Maybe.map Encode.string >> Maybe.withDefault Encode.null


encodeOptions : Options -> Encode.Value
encodeOptions =
    Encode.list
        (\( val, label_ ) ->
            Encode.object
                [ ( "value", Encode.string val )
                , ( "label", Encode.string label_ )
                ]
        )



-- HELPERS


makeField : Field -> Element
makeField field =
    FieldElement
        { id = Id.unset
        , field = field
        , name = Nothing
        , isRequired = False
        , label = Nothing
        , placeholder = Nothing
        , hint = Nothing
        , help = Nothing
        , drag = Drag.idle
        }



-- UPDATE


updateIds : Id -> Element -> ( Id, Element )
updateIds currentId element =
    Tuple.mapFirst Id.increment <|
        case element of
            ElementGroup params ->
                updateIdsHelp currentId params
                    |> Tuple.mapSecond ElementGroup

            RepeatableGroup params ->
                updateIdsHelp currentId params
                    |> Tuple.mapSecond RepeatableGroup

            FieldElement field ->
                ( currentId, FieldElement { field | id = currentId } )

            Review field ->
                ( currentId, Review { field | id = currentId } )

            Help field ->
                ( currentId, Help { field | id = currentId } )

            Blank _ ->
                ( currentId, Blank currentId )


updateIdsHelp :
    Id
    -> { branch | elements : List Element, id : Id }
    -> ( Id, { branch | elements : List Element, id : Id } )
updateIdsHelp currentId params =
    List.foldr updateIdsFoldFn ( Id.increment currentId, [] ) params.elements
        |> Tuple.mapSecond (\els -> { params | id = currentId, elements = els })


updateIdsFoldFn : Element -> ( Id, List Element ) -> ( Id, List Element )
updateIdsFoldFn el ( i, els ) =
    updateIds i el
        |> Tuple.mapFirst Id.increment
        |> Tuple.mapSecond (flip (::) els)


prepend : Element -> Element -> Element
prepend prepended element =
    case element of
        ElementGroup params ->
            ElementGroup { params | elements = prepended :: params.elements }

        RepeatableGroup params ->
            RepeatableGroup { params | elements = prepended :: params.elements }

        _ ->
            element



-- List Folds


map : (Element -> Element) -> Element -> Element
map func element =
    let
        mapFunc =
            List.map (map func)
    in
    case element of
        ElementGroup params ->
            func <|
                ElementGroup { params | elements = mapFunc params.elements }

        RepeatableGroup params ->
            func <|
                RepeatableGroup { params | elements = mapFunc params.elements }

        _ ->
            func element


concatMap : (Element -> List Element) -> Element -> Element
concatMap func element =
    let
        mapFunc =
            List.concatMap (concatMap func >> func)
    in
    case element of
        ElementGroup params ->
            ElementGroup { params | elements = mapFunc params.elements }

        RepeatableGroup params ->
            RepeatableGroup { params | elements = mapFunc params.elements }

        _ ->
            element


foldl : (Element -> b -> b) -> b -> Element -> b
foldl func acc element =
    let
        foldGroup =
            List.foldl (flip (foldl func))
    in
    case element of
        ElementGroup params ->
            foldGroup acc params.elements

        RepeatableGroup params ->
            foldGroup acc params.elements

        _ ->
            func element acc
