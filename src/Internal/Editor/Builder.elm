module Internal.Editor.Builder exposing
    ( Model, Msg, init, update, withElement
    , view, viewCompact
    , preview
    )

{-| The drag-and-drop form builder: a palette of fields to drag in, the tree of
elements, and a form for editing the properties of the selected element.

The builder is a self-contained component. Keep its `Model` in your own model,
feed it the `Msg`s produced by its views, and run the `Cmd`s [update](#update)
returns. Render it with [view](#view) or [viewCompact](#viewCompact).


# Builder

@docs Model, Msg, init, update, withElement


# Views

@docs view, viewCompact


# Previewing

@docs preview

-}

import Browser.Dom as Dom
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Value as Value
import Html exposing (Attribute, Html, aside, button, div, h3, i, section, text)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Events as Events exposing (on, onClick, onMouseDown, stopPropagationOn)
import Internal.Editor.Drag as Drag exposing (Drag, Position(..))
import Internal.Editor.Element as Element exposing (Element(..))
import Internal.Editor.Form as EditorForm
import Internal.Editor.Id as Id exposing (Id)
import Json.Decode as Decode exposing (Decoder)
import Task


type alias Model =
    { element : Element
    , nextNode : Id
    , selected : Maybe Element
    , paramsOpen : Bool
    , flagged : List Id
    , dragAction : DragAction
    , editForm : EditorForm.EditorForm
    }


type DragAction
    = None
    | Add Element
    | Move Element


type Msg
    = ElementSelected Element
    | OpenToggled Id
    | DragEnabled Id
    | DragStarted DragAction
    | DraggedOver Bool Id Position
    | DroppedOver
    | DragEnded
    | ElementRemoved Id
    | FormMsg EditorForm.Msg
    | Deselect
    | NoOp


type alias PreviewId =
    ()


init : Model
init =
    let
        ( nextId, rootElement ) =
            Element.updateIds (Id.fromInt 1)
                (Element.root
                    [ ElementGroup
                        { id = Id.unset
                        , attributes = [ Element.Name "fields" ]
                        , elements =
                            [ FieldElement
                                { id = Id.unset
                                , field = Element.TextField
                                , attributes = [ Element.Name "text_field" ]
                                , drag = Drag.idle
                                }
                            ]
                        , isOpen = True
                        , drag = Drag.idle
                        }
                    ]
                )
    in
    { element = rootElement
    , nextNode = nextId
    , selected = Nothing
    , paramsOpen = False
    , flagged = []
    , dragAction = None
    , editForm = EditorForm.init (Element.root [])
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ElementSelected element ->
            ( { model
                | selected = Just element
                , paramsOpen = True
                , editForm = EditorForm.init element
              }
            , focusLabel element
            )

        OpenToggled elementId ->
            ( { model | element = toggleOpen elementId model.element }, Cmd.none )

        DragEnabled elementId ->
            ( { model | element = dragChanged elementId Drag.Enabled model.element }, Cmd.none )

        DragStarted dragAction ->
            let
                id =
                    draggedElement dragAction
                        |> Maybe.map Element.id
                        |> Maybe.withDefault Id.unset
            in
            ( { model
                | element = dragChanged id Drag.Dragged model.element
                , dragAction = dragAction
              }
            , Cmd.none
            )

        DraggedOver isTopLevel containerId position ->
            ( { model
                | element =
                    draggedOver isTopLevel
                        containerId
                        position
                        (draggedElement model.dragAction)
                        model.element
              }
            , Cmd.none
            )

        DroppedOver ->
            case model.dragAction of
                Add element ->
                    let
                        ( nextId, element_ ) =
                            Element.updateIds model.nextNode element
                    in
                    ( { model
                        | element = droppedOver element_ model.element
                        , selected = Just element_
                        , editForm = EditorForm.init element_
                        , dragAction = None
                        , nextNode = nextId
                      }
                    , Cmd.none
                    )

                Move element ->
                    ( { model
                        | element = droppedOver element model.element
                        , selected = Just element
                        , editForm = EditorForm.init element
                        , dragAction = None
                      }
                    , Cmd.none
                    )

                None ->
                    ( model, Cmd.none )

        DragEnded ->
            ( { model
                | element = dragEnded model.element
                , dragAction = None
              }
            , Cmd.none
            )

        ElementRemoved elementId ->
            ( { model
                | element = remove elementId model.element
                , selected = Nothing
                , paramsOpen = False
              }
            , Cmd.none
            )

        FormMsg formMsg ->
            let
                ( newForm, resultUpdatedElement ) =
                    EditorForm.update formMsg model.editForm

                flagged =
                    flaggedFor newForm model.flagged
            in
            ( case resultUpdatedElement of
                Ok updatedElement ->
                    { model
                        | editForm = newForm
                        , element = updateElementInTree updatedElement model.element
                        , selected = Just updatedElement
                        , flagged = flagged
                    }

                Err _ ->
                    { model | editForm = newForm, flagged = flagged }
            , Cmd.none
            )

        Deselect ->
            ( { model | selected = Nothing, paramsOpen = False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


focusLabel : Element -> Cmd Msg
focusLabel element =
    case EditorForm.labelInputId element of
        Just inputId ->
            Task.attempt (\_ -> NoOp) (Dom.focus inputId)

        Nothing ->
            Cmd.none


{-| Start from a different element tree: ids are assigned freshly and nothing is
selected.
-}
withElement : Element -> Model -> Model
withElement element model =
    let
        ( nextId, element_ ) =
            Element.updateIds (Id.fromInt 1) element
    in
    { model
        | element = element_
        , nextNode = nextId
        , selected = Nothing
        , paramsOpen = False
        , flagged = []
        , dragAction = None
        , editForm = EditorForm.init (Element.root [])
    }


{-| The form holds a snapshot from selection time: the tree keeps its own
state, and only the properties the form edits are taken from it.
-}
updateElementInTree : Element -> Element -> Element
updateElementInTree updatedElement =
    Element.map
        (\element ->
            if Element.id element == Element.id updatedElement then
                mergeTreeState element updatedElement

            else
                element
        )


mergeTreeState : Element -> Element -> Element
mergeTreeState treeElement updatedElement =
    case ( treeElement, updatedElement ) of
        ( ElementGroup from, ElementGroup to ) ->
            ElementGroup { to | id = from.id, drag = from.drag, isOpen = from.isOpen, elements = from.elements }

        ( RepeatableGroup from, RepeatableGroup to ) ->
            RepeatableGroup { to | id = from.id, drag = from.drag, isOpen = from.isOpen, elements = from.elements }

        _ ->
            updatedElement
                |> Element.withId (Element.id treeElement)
                |> Element.updateDrag (Element.drag treeElement)


dragChanged : Id -> Drag -> Element -> Element
dragChanged elementId drag =
    Element.map
        (\element ->
            if Element.id element == elementId then
                Element.updateDrag drag element

            else
                element
        )


draggedElement : DragAction -> Maybe Element
draggedElement dragAction =
    case dragAction of
        None ->
            Nothing

        Add element ->
            Just element

        Move element ->
            Just element


draggedOver : Bool -> Id -> Position -> Maybe Element -> Element -> Element
draggedOver isTopLevel id position dragged editorRoot =
    case dragged of
        Just element ->
            let
                mapFunc =
                    draggedOverHelp
                        (dragged |> Maybe.map descendantIds |> Maybe.withDefault [])
                        id
                        position
            in
            if not (Element.isGroup element) && isTopLevel then
                Element.concatMap (mapFunc Nothing) editorRoot

            else
                Element.concatMap
                    (mapFunc (Just Element.placeholder))
                    editorRoot

        Nothing ->
            editorRoot


{-| Dropping a node inside itself would remove it.
-}
descendantIds : Element -> List Id
descendantIds element =
    List.concatMap
        (\child -> Element.id child :: descendantIds child)
        (Element.elements element)


draggedOverHelp : List Id -> Id -> Position -> Maybe Element -> Element -> List Element
draggedOverHelp invalidTargets containerId position placeholder element =
    if List.member (Element.id element) invalidTargets then
        [ element ]

    else if Element.id element == containerId then
        if Element.isPlaceholder element then
            [ element ]

        else if Element.isEmptyGroup element then
            -- an empty group only ever offers "drop into me" (the
            -- groupPlaceholder below); it must not also gain a sibling
            -- placeholder, or a drop could match either one and duplicate
            [ element
                |> Element.open True
                |> Element.prepend Element.groupPlaceholder
            ]

        else
            addPlaceholder position placeholder (Element.open True element)

    else if Element.isPlaceholder element then
        []

    else
        [ element ]


addPlaceholder : Position -> Maybe Element -> Element -> List Element
addPlaceholder position placeholder element =
    List.filterMap identity <|
        case position of
            Before ->
                [ placeholder, Just element ]

            After ->
                [ Just element, placeholder ]


droppedOver : Element -> Element -> Element
droppedOver dropped editorRoot =
    if Element.isEmptyGroup editorRoot then
        Element.prepend dropped editorRoot

    else if hasPlaceholder editorRoot then
        editorRoot
            |> Element.concatMap
                (\element ->
                    if Element.isPlaceholder element then
                        [ dropped ]

                    else if Element.id element == Element.id dropped then
                        []

                    else
                        [ element ]
                )

    else
        editorRoot


hasPlaceholder : Element -> Bool
hasPlaceholder =
    Element.foldl (\element acc -> acc || Element.isPlaceholder element) False


dragEnded : Element -> Element
dragEnded =
    Element.concatMap
        (\element ->
            if Element.isPlaceholder element then
                []

            else
                [ Element.updateDrag Drag.idle element ]
        )
        >> Element.updateDrag Drag.idle


toggleOpen : Id -> Element -> Element
toggleOpen elementId =
    Element.map
        (\element ->
            if Element.id element == elementId then
                Element.toggleOpen element

            else
                element
        )


remove : Id -> Element -> Element
remove elementId =
    Element.concatMap
        (\element ->
            if Element.id element == elementId then
                []

            else
                [ element ]
        )


{-| The form the tree describes, with every field disabled: a live preview that
cannot be typed into. Its (unit) messages are never sent, so map them away in
your own view.
-}
preview : Model -> Html ()
preview model =
    Element.elements model.element
        |> List.filterMap elementToPreviewField
        |> Field.group []
        |> Field.toHtml identity
        |> Html.map (\_ -> ())


elementToPreviewField : Element -> Maybe (Field PreviewId)
elementToPreviewField element =
    if EditorForm.isValid element then
        buildPreviewField element

    else
        Nothing


buildPreviewField : Element -> Maybe (Field PreviewId)
buildPreviewField element =
    case element of
        FieldElement params ->
            Just
                (case params.field of
                    Element.TextField ->
                        Field.text
                            [ Field.label (Element.labelAttribute params.attributes |> Maybe.withDefault "")
                            , Field.placeholder (Element.placeholderAttribute params.attributes |> Maybe.withDefault "")
                            , Field.hint (Element.hintAttribute params.attributes |> Maybe.withDefault "")
                            , Field.required (Element.requiredAttribute params.attributes)
                            , Field.disabled True
                            ]

                    Element.Checkbox ->
                        Field.checkbox
                            [ Field.label (Element.labelAttribute params.attributes |> Maybe.withDefault "")
                            , Field.hint (Element.hintAttribute params.attributes |> Maybe.withDefault "")
                            , Field.disabled True
                            ]

                    Element.IntegerField { min, max } ->
                        Field.int
                            [ Field.label (Element.labelAttribute params.attributes |> Maybe.withDefault "")
                            , Field.placeholder (Element.placeholderAttribute params.attributes |> Maybe.withDefault "")
                            , Field.hint (Element.hintAttribute params.attributes |> Maybe.withDefault "")
                            , Field.min min
                            , Field.max max
                            , Field.required (Element.requiredAttribute params.attributes)
                            , Field.disabled True
                            ]

                    Element.DateField { min, max } ->
                        Field.date
                            [ Field.label (Element.labelAttribute params.attributes |> Maybe.withDefault "")
                            , Field.hint (Element.hintAttribute params.attributes |> Maybe.withDefault "")
                            , Field.min min
                            , Field.max max
                            , Field.required (Element.requiredAttribute params.attributes)
                            , Field.disabled True
                            ]

                    Element.MonthField { min, max } ->
                        Field.month
                            [ Field.label (Element.labelAttribute params.attributes |> Maybe.withDefault "")
                            , Field.hint (Element.hintAttribute params.attributes |> Maybe.withDefault "")
                            , Field.min min
                            , Field.max max
                            , Field.required (Element.requiredAttribute params.attributes)
                            , Field.disabled True
                            ]

                    Element.Select options ->
                        Field.select
                            [ Field.label (Element.labelAttribute params.attributes |> Maybe.withDefault "")
                            , Field.placeholder (Element.placeholderAttribute params.attributes |> Maybe.withDefault "")
                            , Field.hint (Element.hintAttribute params.attributes |> Maybe.withDefault "")
                            , Field.options (List.map (\( v, l ) -> ( l, Value.string v )) options)
                            , Field.required (Element.requiredAttribute params.attributes)
                            , Field.disabled True
                            ]

                    Element.Radio options ->
                        Field.radio
                            [ Field.label (Element.labelAttribute params.attributes |> Maybe.withDefault "")
                            , Field.hint (Element.hintAttribute params.attributes |> Maybe.withDefault "")
                            , Field.options (List.map (\( v, l ) -> ( l, Value.string v )) options)
                            , Field.required (Element.requiredAttribute params.attributes)
                            , Field.disabled True
                            ]
                )

        ElementGroup params ->
            let
                children =
                    List.filterMap elementToPreviewField params.elements
            in
            if List.isEmpty children then
                Nothing

            else
                Just (Field.group (previewGroupAttributes params) children)

        RepeatableGroup params ->
            let
                children =
                    List.filterMap elementToPreviewField params.elements
            in
            if List.isEmpty children then
                Nothing

            else
                Just (Field.group (previewGroupAttributes params) children)

        Review params ->
            Just
                (Field.textarea
                    [ Field.label (Element.nameAttribute params.attributes |> Maybe.withDefault "")
                    , Field.value (Value.string (Element.textAttribute params.attributes |> Maybe.withDefault ""))
                    , Field.disabled True
                    ]
                )

        Help params ->
            Just
                (Field.text
                    [ Field.label (Element.buttonAttribute params.attributes |> Maybe.withDefault "")
                    , Field.value (Value.string (Element.textAttribute params.attributes |> Maybe.withDefault ""))
                    , Field.disabled True
                    ]
                )

        Blank _ ->
            Nothing


previewGroupAttributes : { a | attributes : List Element.Attribute } -> List (Field.Attribute PreviewId val)
previewGroupAttributes params =
    Field.label (Element.labelAttribute params.attributes |> Maybe.withDefault "")
        :: (if Element.inlineAttribute params.attributes then
                [ Field.inline ]

            else
                []
           )


type alias TreeContext =
    { selected : Maybe Element
    , flagged : List Id
    }


{-| The builder in three columns: palette, tree and the properties of the
selected element.
-}
view : Model -> Html Msg
view model =
    div [ class "schema-editor", onClick Deselect ]
        [ section [ class "schema-editor-panes" ]
            [ viewPalette
            , viewTree model
            , viewParams model
            ]
        ]


{-| The builder in two columns, palette and tree, for narrower places: the
properties of the selected element slide in over the palette, and clicking
outside the panel puts them away.
-}
viewCompact : Model -> Html Msg
viewCompact model =
    div [ class "schema-editor schema-editor--compact", onClick Deselect ]
        [ section [ class "schema-editor-panes" ]
            [ viewPalette
            , viewTree model
            ]
        , viewParams model
        ]


viewPalette : Html Msg
viewPalette =
    div [ class "schema-editor-palete" ]
        [ addFieldHtml Element.text
        , addFieldHtml Element.checkbox
        , addFieldHtml Element.integer
        , addFieldHtml Element.date
        , addFieldHtml Element.month
        , addFieldHtml (Element.select [])
        , addFieldHtml (Element.radio [])
        , addElementHtml "repeatable-group-addition" Element.repeatableGroup
        , addElementHtml "group-addition" Element.group
        ]


viewTree : Model -> Html Msg
viewTree model =
    div [ class "schema-editor-tree" ]
        (if Element.isEmptyGroup model.element then
            [ placeholderHtml model.element ]

         else
            let
                context =
                    { selected = model.selected, flagged = model.flagged }
            in
            model.element
                |> Element.elements
                |> List.map (elementToHtml context True)
        )


viewParams : Model -> Html Msg
viewParams model =
    aside
        [ classList
            [ ( "side-pane", True )
            , ( "is-open", model.paramsOpen )
            ]
        , stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
        ]
        (case model.selected of
            Just element ->
                [ h3 [] [ text "Edit Element" ]
                , Html.map FormMsg (EditorForm.view model.editForm)
                , button [ onClick (ElementRemoved (Element.id element)), class "button" ]
                    [ text "Remove Element" ]
                ]

            Nothing ->
                [ text "Select an element to edit" ]
        )


addFieldHtml : Element -> Html Msg
addFieldHtml element =
    addElementHtml "field-addition" element


addElementHtml : String -> Element -> Html Msg
addElementHtml class_ element =
    div
        [ class class_
        , class "editor-add"
        , Attributes.draggable "true"
        , on "dragend" <| Decode.succeed DragEnded
        , on "dragstart" <|
            Decode.succeed (DragStarted (Add element))
        ]
        [ div [ class "drag-handle" ] [ icon "gg-layout-grid-small" ]
        , elementIcon element
        , h3 [] [ text (Element.label element) ]
        ]


elementToHtml : TreeContext -> Bool -> Element -> Html Msg
elementToHtml context isTopLevel element =
    case element of
        ElementGroup params ->
            div
                (class "element group-element"
                    :: identifier element
                    :: dragAttributes
                        { isTopLevel = isTopLevel
                        , selected = context.selected
                        , draggable = element
                        , container = element
                        }
                )
                [ div [ class "group" ]
                    (groupHtmlContent context element params)
                ]

        RepeatableGroup params ->
            div
                (class "element group-element"
                    :: identifier element
                    :: dragAttributes
                        { isTopLevel = isTopLevel
                        , selected = context.selected
                        , draggable = element
                        , container = element
                        }
                )
                [ div [ class "repeatable-group" ]
                    (groupHtmlContent context element params)
                ]

        FieldElement _ ->
            fieldHtml context
                { nodeClass = "field-element"
                , isTopLevel = isTopLevel
                , element = element
                }

        Review _ ->
            fieldHtml context
                { nodeClass = "review-element"
                , isTopLevel = isTopLevel
                , element = element
                }

        Help _ ->
            fieldHtml context
                { nodeClass = "help-element"
                , isTopLevel = isTopLevel
                , element = element
                }

        Blank _ ->
            placeholderHtml element


fieldHtml :
    TreeContext
    ->
        { nodeClass : String
        , isTopLevel : Bool
        , element : Element
        }
    -> Html Msg
fieldHtml context { nodeClass, isTopLevel, element } =
    div
        (class "element"
            :: class nodeClass
            :: identifier element
            :: dragAttributes
                { isTopLevel = isTopLevel
                , selected = context.selected
                , draggable = element
                , container = element
                }
        )
        [ div
            [ class "field"
            , class (Element.elementType element)
            ]
            [ dragHandle element
            , div
                [ class "field-content"
                , stopPropagationOn "click"
                    (Decode.succeed ( ElementSelected element, True ))
                ]
                [ elementIcon element
                , h3 [] [ text (Element.label element) ]
                , warningIcon context element
                ]
            ]
        ]


groupHtmlContent :
    TreeContext
    -> Element
    -> { a | attributes : List Element.Attribute, elements : List Element, isOpen : Bool }
    -> List (Html Msg)
groupHtmlContent context element { attributes, elements, isOpen } =
    [ dragHandle element
    , div
        [ class "group-content"
        , stopPropagationOn "click"
            (Decode.succeed ( ElementSelected element, True ))
        ]
        [ h3 [ class "group-name" ]
            [ button
                [ class "button-clear"
                , onClick (OpenToggled (Element.id element))
                ]
                [ if isOpen then
                    icon "gg-chevron-down"

                  else
                    icon "gg-chevron-right"
                ]
            , text (Element.label element)
            , warningIcon context element
            ]
        , div
            [ class "group-fields"
            , classList [ ( "inline", Element.inlineAttribute attributes ), ( "stacked", not (Element.inlineAttribute attributes) ) ]
            ]
            (if isOpen then
                List.map (elementToHtml context False) elements

             else
                []
            )
        ]
    ]


placeholderHtml : Element -> Html Msg
placeholderHtml element =
    div
        (class "element placeholder-element"
            :: dragAttributes
                { isTopLevel = False
                , selected = Nothing
                , draggable = element
                , container = element
                }
        )
        [ div [ class "placeholder" ] [ text "" ] ]


dragAttributes :
    { isTopLevel : Bool
    , selected : Maybe Element
    , draggable : Element
    , container : Element
    }
    -> List (Attribute Msg)
dragAttributes { isTopLevel, selected, draggable, container } =
    let
        draggableId =
            Element.id draggable

        selectionClass =
            if Maybe.map Element.id selected == Just draggableId then
                class "element-selected"

            else
                class ""
    in
    case Element.drag draggable of
        Drag.Idle ->
            let
                containerId =
                    Element.id container
            in
            [ customOn "dragover"
                (Decode.map (DraggedOver isTopLevel containerId)
                    Drag.positionDecoder
                )
            , customOn "dragenter"
                (Decode.map (DraggedOver isTopLevel containerId)
                    Drag.positionDecoder
                )
            , customOn "drop" (Decode.succeed DroppedOver)
            , on "dragend" (Decode.succeed DragEnded)
            , class "drag-idle"
            , selectionClass
            ]

        Drag.Enabled ->
            [ Attributes.draggable "true"
            , on "dragstart" <| Decode.succeed (DragStarted (Move draggable))
            , on "dragend" (Decode.succeed DragEnded)
            , customOn "drop" (Decode.succeed DroppedOver)
            , class "drag-enabled"
            , selectionClass
            ]

        Drag.Dragged ->
            [ Attributes.draggable "true"
            , on "dragend" (Decode.succeed DragEnded)
            , class "drag-dragged"
            ]


dragHandle : Element -> Html Msg
dragHandle element =
    div
        (class "drag-handle"
            :: (case Element.drag element of
                    Drag.Idle ->
                        [ onMouseDown (DragEnabled (Element.id element)) ]

                    Drag.Enabled ->
                        -- a plain click (mousedown without a following native
                        -- drag) must not leave the element stuck Enabled,
                        -- since Enabled elements aren't valid drop targets
                        [ Events.on "mouseup" (Decode.succeed DragEnded) ]

                    Drag.Dragged ->
                        []
               )
        )
        [ icon "gg-layout-grid-small" ]


elementIcon : Element -> Html Msg
elementIcon element =
    div
        [ class "field-icon"
        , class (Element.elementType element)
        ]
        [ icon (Element.icon element) ]


{-| Only an edited form flags its node: one that was just dropped in is not
wrong yet, and one that parses is not wrong at all.
-}
flaggedFor : EditorForm.EditorForm -> List Id -> List Id
flaggedFor form flagged =
    if not form.touched then
        flagged

    else if form.valid then
        List.filter (\elementId -> elementId /= Element.id form.element) flagged

    else if List.member (Element.id form.element) flagged then
        flagged

    else
        Element.id form.element :: flagged


warningIcon : TreeContext -> Element -> Html Msg
warningIcon context element =
    if List.member (Element.id element) context.flagged then
        icon "warning-icon"

    else
        text ""


identifier : Element -> Attribute Msg
identifier element =
    Element.identifier element
        |> Maybe.map Attributes.id
        |> Maybe.withDefault (class "")


icon : String -> Html Msg
icon iconClass =
    i [ class iconClass ] []


customOn : String -> Decoder Msg -> Attribute Msg
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
