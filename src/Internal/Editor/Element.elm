module Internal.Editor.Element exposing
    ( Element(..), Field(..), FieldParams, Options
    , Attribute(..)
    , nameAttribute, labelAttribute, placeholderAttribute, hintAttribute, helpTextAttribute
    , buttonAttribute, textAttribute, requiredAttribute, inlineAttribute
    , root, text, checkbox, integer, date, month, select, radio, group, repeatableGroup
    , placeholder, groupPlaceholder
    , id, identifier, label, icon, elementType, drag, elements
    , isEmptyGroup, isGroup, isPlaceholder
    , withId, open, toggleOpen, updateDrag, updateIds
    , map, concatMap, foldl, prepend
    , encode, decode
    )

{-| The form builder's element tree: the fields and groups the drag-and-drop
editor manipulates and its JSON codec. This is the canonical schema the
`FormToolkit.Field` form is materialized from, not the other way around, so
this module does not depend on `FormToolkit.Field`.


# Element tree

@docs Element, Field, FieldParams, Options


# Attributes

@docs Attribute
@docs nameAttribute, labelAttribute, placeholderAttribute, hintAttribute, helpTextAttribute
@docs buttonAttribute, textAttribute, requiredAttribute, inlineAttribute


# Field and group constructors

@docs root, text, checkbox, integer, date, month, select, radio, group, repeatableGroup


# Placeholders

@docs placeholder, groupPlaceholder


# Reading an element

@docs id, identifier, label, icon, elementType, drag, elements
@docs isEmptyGroup, isGroup, isPlaceholder


# Updating an element

@docs withId, open, toggleOpen, updateDrag, updateIds


# Tree traversal

@docs map, concatMap, foldl, prepend


# Serialization

@docs encode, decode

-}

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import FormToolkit.Value as Value exposing (Value(..))
import Internal.Editor.Drag as Drag exposing (Drag)
import Internal.Editor.Id as Id exposing (Id)
import Internal.Value as InternalValue
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
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
    { field : Field
    , attributes : List Attribute
    , id : Id
    , drag : Drag
    }


{-| A property of an element.

  - `Name`, `Label`, `Placeholder`, `Hint` — the input's name, label,
    placeholder, and hint text.
  - `HelpText` — the field's help text: kept with the element, but not
    rendered into the form.
  - `Text`, `Button` — the body of a `Review`, and the body and button text of
    a `Help`.
  - `Required` — whether the input has to be filled in.
  - `Inline` — whether a group lays its fields out next to each other.

An element carries at most one variant per property; a property with no
variant is unset.

-}
type Attribute
    = Name String
    | Label String
    | Placeholder String
    | Hint String
    | HelpText String
    | Button String
    | Text String
    | Required Bool
    | Inline Bool


{-| The first attribute for which `getter` succeeds, if any.
-}
attribute : (Attribute -> Maybe val) -> List Attribute -> Maybe val
attribute getter attributes =
    List.head (List.filterMap getter attributes)


type Element
    = FieldElement FieldParams
    | Review
        { attributes : List Attribute
        , id : Id
        , drag : Drag
        }
    | Help
        { attributes : List Attribute
        , id : Id
        , drag : Drag
        }
    | RepeatableGroup
        { attributes : List Attribute
        , elements : List Element
        , id : Id
        , drag : Drag
        , isOpen : Bool
        }
    | ElementGroup
        { attributes : List Attribute
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
        , attributes = []
        , elements = []
        , isOpen = False
        , drag = Drag.idle
        }


repeatableGroup : Element
repeatableGroup =
    RepeatableGroup
        { id = Id.unset
        , attributes = []
        , elements = []
        , drag = Drag.idle
        , isOpen = True
        }


root : List Element -> Element
root children =
    ElementGroup
        { id = Id.unset
        , attributes = [ Name "root" ]
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
    Id.toIdentifier (name element) (id element)


{-| The [Name](#Attribute) of an element, if it has one.
-}
name : Element -> Maybe String
name element =
    case element of
        ElementGroup params ->
            nameAttribute params.attributes

        RepeatableGroup params ->
            nameAttribute params.attributes

        FieldElement params ->
            nameAttribute params.attributes

        Review params ->
            nameAttribute params.attributes

        Help params ->
            nameAttribute params.attributes

        Blank _ ->
            Nothing


label : Element -> String
label element =
    case element of
        ElementGroup params ->
            labelAttribute params.attributes
                |> Maybe.withDefault "Group"

        RepeatableGroup params ->
            labelAttribute params.attributes
                |> Maybe.withDefault "Repeatable"

        FieldElement params ->
            labelAttribute params.attributes
                |> Maybe.withDefault (String.toSentenceCase (elementType element))

        Review params ->
            nameAttribute params.attributes
                |> Maybe.withDefault "Review"

        Help params ->
            buttonAttribute params.attributes
                |> Maybe.withDefault "Help"

        Blank _ ->
            ""


{-| Read one property off an element's attribute list, with a getter per
[Attribute](#Attribute) variant. `requiredAttribute` and `inlineAttribute` are
`False` when the property is unset.
-}
nameAttribute : List Attribute -> Maybe String
nameAttribute =
    attribute
        (\attr ->
            case attr of
                Name name_ ->
                    Just name_

                _ ->
                    Nothing
        )


labelAttribute : List Attribute -> Maybe String
labelAttribute =
    attribute
        (\attr ->
            case attr of
                Label label_ ->
                    Just label_

                _ ->
                    Nothing
        )


placeholderAttribute : List Attribute -> Maybe String
placeholderAttribute =
    attribute
        (\attr ->
            case attr of
                Placeholder placeholder_ ->
                    Just placeholder_

                _ ->
                    Nothing
        )


hintAttribute : List Attribute -> Maybe String
hintAttribute =
    attribute
        (\attr ->
            case attr of
                Hint hint_ ->
                    Just hint_

                _ ->
                    Nothing
        )


helpTextAttribute : List Attribute -> Maybe String
helpTextAttribute =
    attribute
        (\attr ->
            case attr of
                HelpText helpText_ ->
                    Just helpText_

                _ ->
                    Nothing
        )


buttonAttribute : List Attribute -> Maybe String
buttonAttribute =
    attribute
        (\attr ->
            case attr of
                Button button_ ->
                    Just button_

                _ ->
                    Nothing
        )


textAttribute : List Attribute -> Maybe String
textAttribute =
    attribute
        (\attr ->
            case attr of
                Text text_ ->
                    Just text_

                _ ->
                    Nothing
        )


requiredAttribute : List Attribute -> Bool
requiredAttribute =
    attribute
        (\attr ->
            case attr of
                Required value ->
                    Just value

                _ ->
                    Nothing
        )
        >> Maybe.withDefault False


inlineAttribute : List Attribute -> Bool
inlineAttribute =
    attribute
        (\attr ->
            case attr of
                Inline value ->
                    Just value

                _ ->
                    Nothing
        )
        >> Maybe.withDefault False


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
            List.isEmpty params.elements

        RepeatableGroup params ->
            List.isEmpty params.elements

        _ ->
            False


isGroup : Element -> Bool
isGroup element =
    case element of
        ElementGroup _ ->
            True

        RepeatableGroup _ ->
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
            RepeatableGroup { params | isOpen = not params.isOpen }

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


{-| Serialize an element tree to JSON, or `Nothing` for a
[Blank](#Element) placeholder.

Load it back with [decode](#decode). Element ids and the builder's transient
state (selection, drag, collapsed groups) are not saved: a loaded tree has no
ids until you assign them with `Internal.Editor.Builder.withElement`, and its groups are
open.

-}
encode : Element -> Maybe Encode.Value
encode element =
    case element of
        ElementGroup attrs ->
            Just
                (Encode.object
                    ([ ( "type", Encode.string (elementType element) )
                     , ( "fields"
                       , Encode.list identity (List.filterMap encode attrs.elements)
                       )
                     ]
                        ++ encodeAttributes attrs.attributes
                    )
                )

        RepeatableGroup attrs ->
            Just
                (Encode.object
                    ([ ( "type", Encode.string (elementType element) )
                     , ( "fields"
                       , Encode.list identity (List.filterMap encode attrs.elements)
                       )
                     ]
                        ++ encodeAttributes attrs.attributes
                    )
                )

        FieldElement attrs ->
            let
                typePair =
                    ( "type", Encode.string (elementType element) )

                attributePairs =
                    encodeAttributes attrs.attributes

                elementJson extraPairs =
                    Just (Encode.object (typePair :: (extraPairs ++ attributePairs)))
            in
            case attrs.field of
                TextField ->
                    elementJson []

                Checkbox ->
                    elementJson []

                IntegerField { min, max } ->
                    elementJson (rangePair min max)

                DateField { min, max } ->
                    elementJson (rangePair min max)

                MonthField { min, max } ->
                    elementJson (rangePair min max)

                Select options ->
                    elementJson (optionsPair options)

                Radio options ->
                    elementJson (optionsPair options)

        Review attrs ->
            Just
                (Encode.object
                    (( "type", Encode.string (elementType element) )
                        :: encodeAttributes attrs.attributes
                    )
                )

        Help attrs ->
            Just
                (Encode.object
                    (( "type", Encode.string (elementType element) )
                        :: encodeAttributes attrs.attributes
                    )
                )

        Blank _ ->
            Nothing


rangePair : Value -> Value -> List ( String, Encode.Value )
rangePair min max =
    [ ( "min", Encode.string (Maybe.withDefault "" (Value.toString min)) )
    , ( "max", Encode.string (Maybe.withDefault "" (Value.toString max)) )
    ]


optionsPair : Options -> List ( String, Encode.Value )
optionsPair options =
    [ ( "options", encodeOptions options ) ]


{-| The key/value pairs the element's attributes contribute to its object,
in the order the attributes appear.
-}
encodeAttributes : List Attribute -> List ( String, Encode.Value )
encodeAttributes =
    List.map encodeAttribute


{-| The JSON key and value for one attribute.
-}
encodeAttribute : Attribute -> ( String, Encode.Value )
encodeAttribute attr =
    case attr of
        Name s ->
            ( "name", Encode.string s )

        Label s ->
            ( "label", Encode.string s )

        Placeholder s ->
            ( "placeholder", Encode.string s )

        Hint s ->
            ( "hint", Encode.string s )

        HelpText s ->
            ( "help", Encode.string s )

        Button s ->
            ( "button", Encode.string s )

        Text s ->
            ( "text", Encode.string s )

        Required b ->
            ( "required", Encode.bool b )

        Inline b ->
            ( "inline", Encode.bool b )


encodeOptions : Options -> Encode.Value
encodeOptions =
    Encode.list
        (\( val, label_ ) ->
            Encode.object
                [ ( "value", Encode.string val )
                , ( "label", Encode.string label_ )
                ]
        )



-- DECODER


{-| Load an element tree from the JSON produced by [encode](#encode).

The result has no ids yet and its groups are open: hand it to
`Internal.Editor.Builder.withElement` to keep editing it.

-}
decode : Decoder Element
decode =
    Decode.field "type" Decode.string
        |> Decode.andThen decodeByType


decodeByType : String -> Decoder Element
decodeByType typeName =
    case typeName of
        "group" ->
            decodeGroup ElementGroup

        "repeatable-group" ->
            decodeGroup RepeatableGroup

        "text" ->
            decodeField TextField

        "checkbox" ->
            decodeField Checkbox

        "integer" ->
            decodeRange IntegerField InternalValue.intFromString

        "date" ->
            decodeRange DateField InternalValue.dateFromString

        "month" ->
            decodeRange MonthField InternalValue.monthFromString

        "select" ->
            decodeOptions Select

        "radio" ->
            decodeOptions Radio

        "review" ->
            Decode.map
                (\attributes ->
                    Review
                        { id = Id.unset
                        , attributes = attributes
                        , drag = Drag.idle
                        }
                )
                attributesDecoder

        "help" ->
            Decode.map
                (\attributes ->
                    Help
                        { id = Id.unset
                        , attributes = attributes
                        , drag = Drag.idle
                        }
                )
                attributesDecoder

        other ->
            Decode.fail ("Unsupported element type: " ++ other)


decodeGroup : ({ attributes : List Attribute, elements : List Element, id : Id, drag : Drag, isOpen : Bool } -> Element) -> Decoder Element
decodeGroup constructor =
    Decode.map2
        (\attributes children ->
            constructor
                { id = Id.unset
                , attributes = attributes
                , elements = children
                , drag = Drag.idle
                , isOpen = True
                }
        )
        attributesDecoder
        (Decode.field "fields" (Decode.lazy (\_ -> Decode.list decode)))


{-| Decode the [attributes](#Attribute) an element carries.

An absent key and an explicit `null` both mean the attribute is absent; a
present key of the wrong type fails the decode. The result is in canonical
order.

-}
attributesDecoder : Decoder (List Attribute)
attributesDecoder =
    Decode.dict Decode.value
        |> Decode.andThen attributesFromObject


attributesFromObject : Dict String Decode.Value -> Decoder (List Attribute)
attributesFromObject values =
    List.foldr
        (\spec -> Result.map2 (++) (attributeFromObject values spec))
        (Ok [])
        attributeSpecs
        |> (\result ->
                case result of
                    Ok attributes ->
                        Decode.succeed attributes

                    Err message ->
                        Decode.fail message
           )


{-| Every attribute's JSON key and value decoder, in canonical order.
-}
attributeSpecs : List AttributeSpec
attributeSpecs =
    [ stringAttributeSpec "name" Name
    , stringAttributeSpec "label" Label
    , stringAttributeSpec "placeholder" Placeholder
    , stringAttributeSpec "hint" Hint
    , stringAttributeSpec "help" HelpText
    , stringAttributeSpec "button" Button
    , stringAttributeSpec "text" Text
    , boolAttributeSpec "required" Required
    , boolAttributeSpec "inline" Inline
    ]


type alias AttributeSpec =
    { key : String
    , decode : Decode.Value -> Result Decode.Error (Maybe Attribute)
    }


stringAttributeSpec : String -> (String -> Attribute) -> AttributeSpec
stringAttributeSpec key constructor =
    { key = key
    , decode = \value -> Decode.decodeValue (stringOrNull constructor) value
    }


boolAttributeSpec : String -> (Bool -> Attribute) -> AttributeSpec
boolAttributeSpec key constructor =
    { key = key
    , decode = \value -> Decode.decodeValue (boolOrNull constructor) value
    }


{-| `null` means the attribute is absent; a mistyped value is an error rather
than being dropped.
-}
stringOrNull : (String -> Attribute) -> Decoder (Maybe Attribute)
stringOrNull constructor =
    Decode.oneOf
        [ Decode.map (constructor >> Just) Decode.string
        , Decode.null Nothing
        ]


boolOrNull : (Bool -> Attribute) -> Decoder (Maybe Attribute)
boolOrNull constructor =
    Decode.oneOf
        [ Decode.map (constructor >> Just) Decode.bool
        , Decode.null Nothing
        ]


attributeFromObject : Dict String Decode.Value -> AttributeSpec -> Result String (List Attribute)
attributeFromObject values spec =
    case Dict.get spec.key values of
        Nothing ->
            Ok []

        Just value ->
            spec.decode value
                |> Result.mapError (\error -> spec.key ++ ": " ++ Decode.errorToString error)
                |> Result.map
                    (\maybeAttribute ->
                        case maybeAttribute of
                            Just attr ->
                                [ attr ]

                            Nothing ->
                                []
                    )


fieldElementFrom : List Attribute -> Field -> Element
fieldElementFrom attributes field =
    FieldElement
        { id = Id.unset
        , field = field
        , attributes = attributes
        , drag = Drag.idle
        }


decodeField : Field -> Decoder Element
decodeField field =
    Decode.map (\attributes -> fieldElementFrom attributes field) attributesDecoder


decodeRange : ({ min : Value, max : Value } -> Field) -> (String -> InternalValue.Value) -> Decoder Element
decodeRange fieldType fromString =
    Decode.map2
        (\attributes ( min, max ) ->
            fieldElementFrom attributes (fieldType { min = min, max = max })
        )
        attributesDecoder
        (Decode.map2 Tuple.pair
            (rangeValueDecoder "min" fromString)
            (rangeValueDecoder "max" fromString)
        )


rangeValueDecoder : String -> (String -> InternalValue.Value) -> Decoder Value
rangeValueDecoder key fromString =
    Decode.map (Value << fromString) (Decode.field key Decode.string)


decodeOptions : (Options -> Field) -> Decoder Element
decodeOptions fieldType =
    Decode.map2
        (\attributes options -> fieldElementFrom attributes (fieldType options))
        attributesDecoder
        optionsDecoder


optionsDecoder : Decoder Options
optionsDecoder =
    Decode.field "options"
        (Decode.list
            (Decode.map2 Tuple.pair
                (Decode.field "value" Decode.string)
                (Decode.field "label" Decode.string)
            )
        )



-- HELPERS


makeField : Field -> Element
makeField field =
    FieldElement
        { id = Id.unset
        , field = field
        , attributes = []
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
