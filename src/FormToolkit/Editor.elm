module FormToolkit.Editor exposing
    ( Editor, Msg
    , init, update, view
    )

{-| A drag-and-drop form editor that allows building forms visually.


# Core Types

@docs Editor, Msg


# Lifecycle

@docs init, update, view

-}

import Dict exposing (Dict)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Value as Value
import Html exposing (Html, button, div, h3, i, text)
import Html.Attributes exposing (class, classList, draggable, id)
import Html.Events as Events exposing (on, onClick, onMouseDown, stopPropagationOn)
import Internal.Field as Internal exposing (FieldType(..))
import Internal.Value
import Json.Decode as Decode exposing (Decoder)
import RoseTree.Tree as Tree
import Set exposing (Set)


type alias Editor id =
    { field : Field id
    , dragState : Dict String Drag
    , openGroups : Set String
    , selected : Maybe String
    , propertyForm : Maybe (Field String)
    , dragAction : Maybe (DragAction id)
    , nextId : Int
    }


type Drag
    = Idle
    | Enabled
    | Dragged


type Msg id
    = FieldSelected String
    | OpenToggled String
    | DragEnabled String
    | DragStarted (DragAction id)
    | DraggedOver Bool String Position
    | DroppedOver String
    | DragEnded
    | ElementRemoved String
    | PropertyFormChanged (Field.Msg String)
    | PaletteItemDragStarted (Field id)
    | NoOp


type DragAction id
    = AddFromPalette (Field id)
    | MoveExisting String


type Position
    = Before
    | After


init : Editor id
init =
    { field = Field.group [] []
    , dragState = Dict.empty
    , openGroups = Set.empty
    , selected = Nothing
    , propertyForm = Nothing
    , dragAction = Nothing
    , nextId = 1
    }


update : Msg id -> Editor id -> ( Editor id, Cmd (Msg id) )
update msg editor =
    case msg of
        FieldSelected path ->
            ( { editor
                | selected = Just path
                , propertyForm = Just (createPropertyForm editor.field path)
              }
            , Cmd.none
            )

        OpenToggled path ->
            ( { editor
                | openGroups =
                    if Set.member path editor.openGroups then
                        Set.remove path editor.openGroups

                    else
                        Set.insert path editor.openGroups
              }
            , Cmd.none
            )

        DragEnabled path ->
            ( { editor
                | dragState = Dict.insert path Enabled editor.dragState
              }
            , Cmd.none
            )

        DragStarted dragAction ->
            case dragAction of
                MoveExisting path ->
                    ( { editor
                        | dragState = Dict.insert path Dragged editor.dragState
                        , dragAction = Just dragAction
                      }
                    , Cmd.none
                    )

                AddFromPalette _ ->
                    ( { editor | dragAction = Just dragAction }
                    , Cmd.none
                    )

        DraggedOver isTopLevel targetPath position ->
            -- TODO: Insert placeholder elements to show drop target
            ( editor, Cmd.none )

        DroppedOver targetPath ->
            case editor.dragAction of
                Just (AddFromPalette fieldTemplate) ->
                    let
                        (Field.Field tree) =
                            editor.field

                        path =
                            pathFromString targetPath

                        updatedField =
                            insertFieldAtPath path fieldTemplate editor.field
                    in
                    ( { editor
                        | field = updatedField
                        , dragState = Dict.empty
                        , dragAction = Nothing
                        , selected = Just targetPath
                        , propertyForm = Just (createPropertyForm updatedField targetPath)
                      }
                    , Cmd.none
                    )

                Just (MoveExisting sourcePath) ->
                    let
                        sourcePathList =
                            pathFromString sourcePath

                        targetPathList =
                            pathFromString targetPath

                        updatedField =
                            moveField sourcePathList targetPathList editor.field
                    in
                    ( { editor
                        | field = updatedField
                        , dragState = Dict.empty
                        , dragAction = Nothing
                        , selected = Just targetPath
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { editor
                        | dragState = Dict.empty
                        , dragAction = Nothing
                      }
                    , Cmd.none
                    )

        DragEnded ->
            ( { editor
                | dragState = Dict.empty
                , dragAction = Nothing
              }
            , Cmd.none
            )

        ElementRemoved path ->
            let
                pathList =
                    pathFromString path

                updatedField =
                    removeFieldAtPath pathList editor.field
            in
            ( { editor
                | field = updatedField
                , selected = Nothing
                , propertyForm = Nothing
              }
            , Cmd.none
            )

        PropertyFormChanged fieldMsg ->
            case ( editor.propertyForm, editor.selected ) of
                ( Just form, Just path ) ->
                    let
                        updatedForm =
                            Field.update fieldMsg form
                    in
                    ( { editor
                        | propertyForm = Just updatedForm
                        , field = applyPropertyFormToField path updatedForm editor.field
                      }
                    , Cmd.none
                    )

                _ ->
                    ( editor, Cmd.none )

        PaletteItemDragStarted fieldTemplate ->
            ( { editor | dragAction = Just (AddFromPalette fieldTemplate) }
            , Cmd.none
            )

        NoOp ->
            ( editor, Cmd.none )


createPropertyForm : Field id -> String -> Field String
createPropertyForm (Field.Field tree) path =
    let
        pathList =
            pathFromString path

        maybeNode =
            getNodeAtPath pathList tree
    in
    case maybeNode of
        Just node ->
            let
                attrs =
                    Tree.value node
            in
            case attrs.fieldType of
                Text ->
                    textFieldForm attrs

                TextArea ->
                    textAreaFieldForm attrs

                Email ->
                    textFieldForm attrs

                Password ->
                    passwordFieldForm attrs

                StrictAutocomplete ->
                    autocompleteFieldForm attrs

                Integer ->
                    numericFieldForm attrs

                Float ->
                    numericFieldForm attrs

                Date ->
                    dateFieldForm attrs

                Month ->
                    monthFieldForm attrs

                LocalDatetime ->
                    datetimeFieldForm attrs

                Select ->
                    optionsFieldForm attrs

                Radio ->
                    optionsFieldForm attrs

                Checkbox ->
                    checkboxFieldForm attrs

                Group ->
                    groupForm attrs

                Repeatable _ ->
                    repeatableForm attrs

        Nothing ->
            emptyForm


textFieldForm : Internal.Attributes id fieldType value status err -> Field String
textFieldForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.required True
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        , Field.text
            [ Field.name "placeholder"
            , Field.label "Placeholder"
            , Field.value (Value.string (Maybe.withDefault "" attrs.placeholder))
            ]
        , Field.text
            [ Field.name "hint"
            , Field.label "Hint / Help Text"
            , Field.value (Value.string (Maybe.withDefault "" attrs.hint))
            ]
        , Field.checkbox
            [ Field.name "required"
            , Field.label "Required?"
            , Field.value (Value.bool attrs.isRequired)
            ]
        , Field.checkbox
            [ Field.name "disabled"
            , Field.label "Disabled?"
            , Field.value (Value.bool attrs.disabled)
            ]
        ]


textAreaFieldForm : Internal.Attributes id fieldType value status err -> Field String
textAreaFieldForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.required True
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        , Field.text
            [ Field.name "placeholder"
            , Field.label "Placeholder"
            , Field.value (Value.string (Maybe.withDefault "" attrs.placeholder))
            ]
        , Field.text
            [ Field.name "hint"
            , Field.label "Hint / Help Text"
            , Field.value (Value.string (Maybe.withDefault "" attrs.hint))
            ]
        , Field.checkbox
            [ Field.name "required"
            , Field.label "Required?"
            , Field.value (Value.bool attrs.isRequired)
            ]
        , Field.checkbox
            [ Field.name "autogrow"
            , Field.label "Auto-grow Height?"
            , Field.value (Value.bool attrs.autogrow)
            ]
        , Field.checkbox
            [ Field.name "disabled"
            , Field.label "Disabled?"
            , Field.value (Value.bool attrs.disabled)
            ]
        ]


passwordFieldForm : Internal.Attributes id fieldType value status err -> Field String
passwordFieldForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.required True
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        , Field.text
            [ Field.name "placeholder"
            , Field.label "Placeholder"
            , Field.value (Value.string (Maybe.withDefault "" attrs.placeholder))
            ]
        , Field.text
            [ Field.name "hint"
            , Field.label "Hint / Help Text"
            , Field.value (Value.string (Maybe.withDefault "" attrs.hint))
            ]
        , Field.checkbox
            [ Field.name "required"
            , Field.label "Required?"
            , Field.value (Value.bool attrs.isRequired)
            ]
        ]


autocompleteFieldForm : Internal.Attributes id fieldType value status err -> Field String
autocompleteFieldForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.required True
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        , Field.text
            [ Field.name "placeholder"
            , Field.label "Placeholder"
            , Field.value (Value.string (Maybe.withDefault "" attrs.placeholder))
            ]
        , Field.text
            [ Field.name "hint"
            , Field.label "Hint / Help Text"
            , Field.value (Value.string (Maybe.withDefault "" attrs.hint))
            ]
        , Field.checkbox
            [ Field.name "required"
            , Field.label "Required?"
            , Field.value (Value.bool attrs.isRequired)
            ]
        ]


numericFieldForm : Internal.Attributes id fieldType value status err -> Field String
numericFieldForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.required True
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        , Field.text
            [ Field.name "placeholder"
            , Field.label "Placeholder"
            , Field.value (Value.string (Maybe.withDefault "" attrs.placeholder))
            ]
        , Field.text
            [ Field.name "hint"
            , Field.label "Hint / Help Text"
            , Field.value (Value.string (Maybe.withDefault "" attrs.hint))
            ]
        , Field.text
            [ Field.name "min"
            , Field.label "Minimum Value"
            ]
        , Field.text
            [ Field.name "max"
            , Field.label "Maximum Value"
            ]
        , Field.text
            [ Field.name "step"
            , Field.label "Step"
            ]
        , Field.checkbox
            [ Field.name "required"
            , Field.label "Required?"
            , Field.value (Value.bool attrs.isRequired)
            ]
        , Field.checkbox
            [ Field.name "disabled"
            , Field.label "Disabled?"
            , Field.value (Value.bool attrs.disabled)
            ]
        ]


dateFieldForm : Internal.Attributes id fieldType value status err -> Field String
dateFieldForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.required True
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        , Field.text
            [ Field.name "hint"
            , Field.label "Hint / Help Text"
            , Field.value (Value.string (Maybe.withDefault "" attrs.hint))
            ]
        , Field.date
            [ Field.name "min"
            , Field.label "Minimum Date"
            ]
        , Field.date
            [ Field.name "max"
            , Field.label "Maximum Date"
            ]
        , Field.checkbox
            [ Field.name "required"
            , Field.label "Required?"
            , Field.value (Value.bool attrs.isRequired)
            ]
        , Field.checkbox
            [ Field.name "disabled"
            , Field.label "Disabled?"
            , Field.value (Value.bool attrs.disabled)
            ]
        ]


monthFieldForm : Internal.Attributes id fieldType value status err -> Field String
monthFieldForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.required True
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        , Field.text
            [ Field.name "hint"
            , Field.label "Hint / Help Text"
            , Field.value (Value.string (Maybe.withDefault "" attrs.hint))
            ]
        , Field.month
            [ Field.name "min"
            , Field.label "Minimum Month"
            ]
        , Field.month
            [ Field.name "max"
            , Field.label "Maximum Month"
            ]
        , Field.checkbox
            [ Field.name "required"
            , Field.label "Required?"
            , Field.value (Value.bool attrs.isRequired)
            ]
        , Field.checkbox
            [ Field.name "disabled"
            , Field.label "Disabled?"
            , Field.value (Value.bool attrs.disabled)
            ]
        ]


datetimeFieldForm : Internal.Attributes id fieldType value status err -> Field String
datetimeFieldForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.required True
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        , Field.text
            [ Field.name "hint"
            , Field.label "Hint / Help Text"
            , Field.value (Value.string (Maybe.withDefault "" attrs.hint))
            ]
        , Field.datetime
            [ Field.name "min"
            , Field.label "Minimum Date/Time"
            ]
        , Field.datetime
            [ Field.name "max"
            , Field.label "Maximum Date/Time"
            ]
        , Field.checkbox
            [ Field.name "required"
            , Field.label "Required?"
            , Field.value (Value.bool attrs.isRequired)
            ]
        , Field.checkbox
            [ Field.name "disabled"
            , Field.label "Disabled?"
            , Field.value (Value.bool attrs.disabled)
            ]
        ]


optionsFieldForm : Internal.Attributes id fieldType value status err -> Field String
optionsFieldForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.required True
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        , Field.text
            [ Field.name "placeholder"
            , Field.label "Placeholder"
            , Field.value (Value.string (Maybe.withDefault "" attrs.placeholder))
            ]
        , Field.text
            [ Field.name "hint"
            , Field.label "Hint / Help Text"
            , Field.value (Value.string (Maybe.withDefault "" attrs.hint))
            ]
        , Field.checkbox
            [ Field.name "required"
            , Field.label "Required?"
            , Field.value (Value.bool attrs.isRequired)
            ]
        , Field.repeatable
            [ Field.name "options"
            , Field.label "Options"
            , Field.repeatableMin 1
            , Field.copies
                { addFieldsButton = "Add Option"
                , removeFieldsButton = "Remove"
                }
            ]
            (Field.group
                [ Field.class "inline-fields" ]
                [ Field.text
                    [ Field.name "label"
                    , Field.label "Option Label"
                    , Field.required True
                    ]
                , Field.text
                    [ Field.name "value"
                    , Field.label "Option Value"
                    , Field.required True
                    ]
                ]
            )
            []
        , Field.checkbox
            [ Field.name "disabled"
            , Field.label "Disabled?"
            , Field.value (Value.bool attrs.disabled)
            ]
        ]


checkboxFieldForm : Internal.Attributes id fieldType value status err -> Field String
checkboxFieldForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.required True
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        , Field.text
            [ Field.name "hint"
            , Field.label "Hint / Help Text"
            , Field.value (Value.string (Maybe.withDefault "" attrs.hint))
            ]
        , Field.checkbox
            [ Field.name "disabled"
            , Field.label "Disabled?"
            , Field.value (Value.bool attrs.disabled)
            ]
        ]


groupForm : Internal.Attributes id fieldType value status err -> Field String
groupForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        ]


repeatableForm : Internal.Attributes id fieldType value status err -> Field String
repeatableForm attrs =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Name"
            , Field.value (Value.string (Maybe.withDefault "" attrs.name))
            ]
        , Field.text
            [ Field.name "label"
            , Field.label "Label"
            , Field.value (Value.string (Maybe.withDefault "" attrs.label))
            ]
        , Field.int
            [ Field.name "repeatableMin"
            , Field.label "Minimum Items"
            , Field.value (Value.int attrs.repeatableMin)
            , Field.min (Value.int 0)
            ]
        , Field.int
            [ Field.name "repeatableMax"
            , Field.label "Maximum Items"
            , Field.value
                (Maybe.map Value.int attrs.repeatableMax
                    |> Maybe.withDefault (Value.int 0)
                )
            , Field.min (Value.int 0)
            ]
        , Field.text
            [ Field.name "addFieldsButton"
            , Field.label "Add Button Text"
            , Field.value (Value.string attrs.addFieldsButtonCopy)
            ]
        , Field.text
            [ Field.name "removeFieldsButton"
            , Field.label "Remove Button Text"
            , Field.value (Value.string attrs.removeFieldsButtonCopy)
            ]
        ]


emptyForm : Field String
emptyForm =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.label "Field Name"
            , Field.required True
            ]
        ]


applyPropertyFormToField : String -> Field String -> Field id -> Field id
applyPropertyFormToField path propertyForm field =
    -- TODO: Parse values from propertyForm and update field at path
    field


view : Editor id -> Html (Msg id)
view editor =
    div
        [ class "schema-editor-container" ]
        [ viewPalette editor
        , div
            [ class "schema-editor" ]
            [ div
                [ class "schema-editor-panes" ]
                [ div
                    [ class "schema-editor-tree" ]
                    [ viewFieldTree editor ]
                , viewSidePane editor
                ]
            ]
        ]


viewPalette : Editor id -> Html (Msg id)
viewPalette editor =
    div
        [ class "schema-editor-palette" ]
        [ paletteItem (Field.text []) "Text Field"
        , paletteItem (Field.textarea []) "Text Area"
        , paletteItem (Field.int []) "Number"
        , paletteItem (Field.date []) "Date"
        , paletteItem (Field.select []) "Select"
        , paletteItem (Field.radio []) "Radio"
        , paletteItem (Field.checkbox []) "Checkbox"
        , paletteItem (Field.group [] []) "Group"
        , paletteItem (Field.repeatable [] (Field.text []) []) "Repeatable"
        ]


paletteItem : Field id -> String -> Html (Msg id)
paletteItem field label =
    div
        [ class "palette-item"
        , draggable "true"
        , on "dragstart" (Decode.succeed (PaletteItemDragStarted field))
        , on "dragend" (Decode.succeed DragEnded)
        ]
        [ icon "gg-layout-grid-small"
        , text label
        ]


viewFieldTree : Editor id -> Html (Msg id)
viewFieldTree editor =
    let
        (Field.Field tree) =
            editor.field
    in
    div []
        (Tree.children tree
            |> List.indexedMap (\idx child -> viewFieldNode editor True [ idx ] child)
        )


viewFieldNode : Editor id -> Bool -> List Int -> Tree.Tree (Internal.Attributes id (Internal.FieldType id Internal.Value.Value err) Internal.Value.Value Internal.Status err) -> Html (Msg id)
viewFieldNode editor isTopLevel path node =
    let
        pathStr =
            pathToString path

        dragState =
            Dict.get pathStr editor.dragState
                |> Maybe.withDefault Idle

        isSelected =
            editor.selected == Just pathStr

        isOpen =
            Set.member pathStr editor.openGroups

        attrs =
            Tree.value node

        fieldLabel =
            getFieldLabel attrs

        fieldIcon =
            getFieldIcon attrs

        isGroup =
            isGroupField attrs

        hasChildren =
            not (List.isEmpty (Tree.children node))
    in
    div
        ([ class "element"
         , classList
            [ ( "element-selected", isSelected )
            , ( "drag-idle", dragState == Idle )
            , ( "drag-enabled", dragState == Enabled )
            , ( "drag-dragged", dragState == Dragged )
            , ( "group-element", isGroup )
            , ( "field-element", not isGroup )
            ]
         , id pathStr
         ]
            ++ dragAttributes editor isTopLevel pathStr dragState
        )
        [ div
            [ class "field" ]
            [ viewDragHandle pathStr dragState
            , div
                [ class "field-content"
                , stopPropagationOn "click"
                    (Decode.succeed ( FieldSelected pathStr, True ))
                ]
                (if isGroup && hasChildren then
                    [ button
                        [ class "button-clear"
                        , onClick (OpenToggled pathStr)
                        ]
                        [ if isOpen then
                            icon "gg-chevron-down"

                          else
                            icon "gg-chevron-right"
                        ]
                    , icon fieldIcon
                    , h3 [] [ text fieldLabel ]
                    ]

                 else
                    [ icon fieldIcon
                    , h3 [] [ text fieldLabel ]
                    ]
                )
            ]
        , viewChildren editor path node isOpen isGroup
        ]


viewChildren : Editor id -> List Int -> Tree.Tree (Internal.Attributes id (Internal.FieldType id Internal.Value.Value err) Internal.Value.Value Internal.Status err) -> Bool -> Bool -> Html (Msg id)
viewChildren editor parentPath node isOpen isGroup =
    if isGroup && isOpen then
        div
            [ class "group-fields" ]
            (Tree.children node
                |> List.indexedMap
                    (\idx child -> viewFieldNode editor False (parentPath ++ [ idx ]) child)
            )

    else
        text ""


viewDragHandle : String -> Drag -> Html (Msg id)
viewDragHandle path dragState =
    div
        (class "drag-handle"
            :: (if dragState == Idle then
                    [ onMouseDown (DragEnabled path) ]

                else
                    []
               )
        )
        [ icon "gg-layout-grid-small" ]


dragAttributes : Editor id -> Bool -> String -> Drag -> List (Html.Attribute (Msg id))
dragAttributes editor isTopLevel path dragState =
    case dragState of
        Idle ->
            [ customOn "dragover"
                (Decode.map (DraggedOver isTopLevel path) positionDecoder)
            , customOn "dragenter"
                (Decode.map (DraggedOver isTopLevel path) positionDecoder)
            , on "dragend" (Decode.succeed DragEnded)
            ]

        Enabled ->
            [ draggable "true"
            , on "dragstart" (Decode.succeed (DragStarted (MoveExisting path)))
            , on "dragend" (Decode.succeed DragEnded)
            ]

        Dragged ->
            [ draggable "true"
            , on "dragend" (Decode.succeed DragEnded)
            ]


positionDecoder : Decoder Position
positionDecoder =
    Decode.map3 decodePosition
        (Decode.field "currentTarget" dimensionsDecoder)
        (Decode.at [ "currentTarget", "parentNode" ] dimensionsDecoder)
        coordsDecoder


dimensionsDecoder : Decoder { width : Int, height : Int }
dimensionsDecoder =
    Decode.map2 (\w h -> { width = w, height = h })
        (Decode.field "clientWidth" Decode.int)
        (Decode.field "clientHeight" Decode.int)


coordsDecoder : Decoder { x : Int, y : Int }
coordsDecoder =
    Decode.map2 (\x y -> { x = x, y = y })
        (Decode.field "offsetX" Decode.float |> Decode.map round)
        (Decode.field "offsetY" Decode.float |> Decode.map round)


decodePosition : { width : Int, height : Int } -> { width : Int, height : Int } -> { x : Int, y : Int } -> Position
decodePosition target parent { x, y } =
    if target.width <= parent.width // 2 then
        if x < target.width // 4 then
            Before

        else
            After

    else if y < target.height // 2 then
        Before

    else
        After


viewSidePane : Editor id -> Html (Msg id)
viewSidePane editor =
    case ( editor.selected, editor.propertyForm ) of
        ( Just path, Just form ) ->
            div
                [ class "side-pane" ]
                [ Field.toHtml PropertyFormChanged form
                , button
                    [ onClick (ElementRemoved path)
                    , class "button-remove"
                    ]
                    [ text "Remove Element" ]
                ]

        _ ->
            div [ class "side-pane" ] []


getFieldLabel : Internal.Attributes id (Internal.FieldType id value err) value status err -> String
getFieldLabel attrs =
    case attrs.label of
        Just lbl ->
            lbl

        Nothing ->
            case attrs.name of
                Just n ->
                    n

                Nothing ->
                    getFieldTypeName attrs.fieldType


getFieldTypeName : Internal.FieldType id value err -> String
getFieldTypeName fieldType =
    case fieldType of
        Text ->
            "Text Field"

        TextArea ->
            "Text Area"

        Email ->
            "Email"

        Password ->
            "Password"

        StrictAutocomplete ->
            "Autocomplete"

        Integer ->
            "Number (Integer)"

        Float ->
            "Number (Float)"

        Month ->
            "Month"

        Date ->
            "Date"

        LocalDatetime ->
            "Date & Time"

        Select ->
            "Select"

        Radio ->
            "Radio"

        Checkbox ->
            "Checkbox"

        Group ->
            "Group"

        Repeatable _ ->
            "Repeatable"


getFieldIcon : Internal.Attributes id (Internal.FieldType id value err) value status err -> String
getFieldIcon attrs =
    case attrs.fieldType of
        Text ->
            "gg-edit-flip-h"

        TextArea ->
            "gg-edit-markup"

        Email ->
            "gg-mail"

        Password ->
            "gg-lock"

        StrictAutocomplete ->
            "gg-search"

        Integer ->
            "gg-hashtag"

        Float ->
            "gg-hashtag"

        Month ->
            "gg-calendar-dates"

        Date ->
            "gg-calendar-due"

        LocalDatetime ->
            "gg-calendar-two"

        Select ->
            "gg-format-separator"

        Radio ->
            "gg-radio-check"

        Checkbox ->
            "gg-check-r"

        Group ->
            "gg-extension-alt"

        Repeatable _ ->
            "gg-extension-add"


isGroupField : Internal.Attributes id (Internal.FieldType id value err) value status err -> Bool
isGroupField attrs =
    case attrs.fieldType of
        Group ->
            True

        Repeatable _ ->
            True

        _ ->
            False


pathToString : List Int -> String
pathToString path =
    "field-" ++ String.join "-" (List.map String.fromInt path)


pathFromString : String -> List Int
pathFromString str =
    str
        |> String.replace "field-" ""
        |> String.split "-"
        |> List.filterMap String.toInt


insertFieldAtPath : List Int -> Field id -> Field id -> Field id
insertFieldAtPath path newField (Field.Field tree) =
    case path of
        [] ->
            Field.Field tree

        [ index ] ->
            let
                (Field.Field newFieldTree) =
                    newField

                children =
                    Tree.children tree

                updatedChildren =
                    List.concat
                        [ List.take index children
                        , [ newFieldTree ]
                        , List.drop index children
                        ]
            in
            Field.Field (Tree.branch (Tree.value tree) updatedChildren)

        index :: rest ->
            let
                children =
                    Tree.children tree

                updatedChildren =
                    List.indexedMap
                        (\i child ->
                            if i == index then
                                let
                                    (Field.Field updatedChild) =
                                        insertFieldAtPath rest newField (Field.Field child)
                                in
                                updatedChild

                            else
                                child
                        )
                        children
            in
            Field.Field (Tree.branch (Tree.value tree) updatedChildren)


removeFieldAtPath : List Int -> Field id -> Field id
removeFieldAtPath path (Field.Field tree) =
    case path of
        [] ->
            Field.Field tree

        [ index ] ->
            let
                children =
                    Tree.children tree

                updatedChildren =
                    List.concat
                        [ List.take index children
                        , List.drop (index + 1) children
                        ]
            in
            Field.Field (Tree.branch (Tree.value tree) updatedChildren)

        index :: rest ->
            let
                children =
                    Tree.children tree

                updatedChildren =
                    List.indexedMap
                        (\i child ->
                            if i == index then
                                let
                                    (Field.Field updatedChild) =
                                        removeFieldAtPath rest (Field.Field child)
                                in
                                updatedChild

                            else
                                child
                        )
                        children
            in
            Field.Field (Tree.branch (Tree.value tree) updatedChildren)


moveField : List Int -> List Int -> Field id -> Field id
moveField sourcePath targetPath field =
    let
        (Field.Field sourceTree) =
            field

        maybeSourceNode =
            getNodeAtPath sourcePath sourceTree

        fieldWithoutSource =
            removeFieldAtPath sourcePath field
    in
    case maybeSourceNode of
        Just sourceNode ->
            insertFieldAtPath targetPath (Field.Field sourceNode) fieldWithoutSource

        Nothing ->
            field


getNodeAtPath : List Int -> Tree.Tree attrs -> Maybe (Tree.Tree attrs)
getNodeAtPath path tree =
    case path of
        [] ->
            Just tree

        [ index ] ->
            Tree.children tree
                |> List.drop index
                |> List.head

        index :: rest ->
            Tree.children tree
                |> List.drop index
                |> List.head
                |> Maybe.andThen (getNodeAtPath rest)


icon : String -> Html (Msg id)
icon iconClass =
    i [ class iconClass ] []


customOn : String -> Decoder (Msg id) -> Html.Attribute (Msg id)
customOn event decoder =
    Events.custom event <|
        Decode.map
            (\msg ->
                { message = msg
                , stopPropagation = True
                , preventDefault = True
                }
            )
            decoder
