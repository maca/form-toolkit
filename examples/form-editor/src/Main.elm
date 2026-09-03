module Main exposing (main)

import Browser
import Editor.Drag as Drag exposing (Drag(..), Position(..))
import Editor.Element as Element exposing (Element(..))
import Editor.Id as Id exposing (Id)
import EditorForm
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Value as Value
import Html
    exposing
        ( Attribute
        , Html
        , aside
        , button
        , div
        , h3
        , i
        , section
        , text
        )
import Html.Attributes as Attributes
    exposing
        ( class
        , classList
        , href
        , id
        , rel
        , target
        )
import Html.Events as Events
    exposing
        ( on
        , onClick
        , onMouseDown
        , stopPropagationOn
        )
import Json.Decode as Decode exposing (Decoder)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { element : Element
    , nextNode : Id
    , selected : Maybe Element
    , dragAction : DragAction
    , editForm : EditorForm.EditorForm
    , activeTab : Tab
    }


type Tab
    = PreviewTab
    | JsonTab


type PreviewId
    = PreviewId


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
    | TabSwitched Tab
    | NoOp


init : Model
init =
    let
        ( nextId, rootElement ) =
            Element.updateIds (Id.fromInt 1)
                (Element.root
                    [ ElementGroup
                        { id = Id.unset
                        , name = Just "fields"
                        , label = Nothing
                        , inline = False
                        , elements =
                            [ FieldElement
                                { id = Id.unset
                                , field = Element.TextField
                                , name = Just "text_field"
                                , isRequired = False
                                , label = Nothing
                                , placeholder = Nothing
                                , hint = Nothing
                                , help = Nothing
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
    , dragAction = None
    , editForm = EditorForm.init (Element.root [])
    , activeTab = PreviewTab
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ElementSelected element ->
            { model | selected = Just element, editForm = EditorForm.init element }

        OpenToggled elementId ->
            { model | element = toggleOpen elementId model.element }

        DragEnabled elementId ->
            { model | element = dragChanged elementId Drag.Enabled model.element }

        DragStarted dragAction ->
            let
                id =
                    draggedElement dragAction
                        |> Maybe.map Element.id
                        |> Maybe.withDefault Id.unset
            in
            { model
                | element = dragChanged id Drag.Dragged model.element
                , dragAction = dragAction
            }

        DraggedOver isTopLevel containerId position ->
            { model
                | element =
                    draggedOver isTopLevel
                        containerId
                        position
                        (draggedElement model.dragAction)
                        model.element
            }

        DroppedOver ->
            case model.dragAction of
                Add element ->
                    let
                        ( nextId, element_ ) =
                            Element.updateIds model.nextNode element
                    in
                    { model
                        | element = droppedOver element_ model.element
                        , selected = Just element_
                        , editForm = EditorForm.init element_
                        , dragAction = None
                        , nextNode = nextId
                    }

                Move element ->
                    { model
                        | element = droppedOver element model.element
                        , selected = Just element
                        , editForm = EditorForm.init element
                        , dragAction = None
                    }

                None ->
                    model

        DragEnded ->
            { model
                | element = dragEnded model.element
                , dragAction = None
            }

        ElementRemoved elementId ->
            { model
                | element = remove elementId model.element
                , selected = Nothing
            }

        FormMsg formMsg ->
            let
                ( newForm, resultUpdatedElement ) =
                    EditorForm.update formMsg model.editForm
            in
            case resultUpdatedElement of
                Ok updatedElement ->
                    { model
                        | editForm = newForm
                        , element = updateElementInTree updatedElement model.element
                        , selected = Just updatedElement
                    }

                Err _ ->
                    { model | editForm = newForm }

        TabSwitched tab ->
            { model | activeTab = tab }

        NoOp ->
            model


updateElementInTree : Element -> Element -> Element
updateElementInTree updatedElement =
    Element.map
        (\element ->
            if Element.id element == Element.id updatedElement then
                updatedElement

            else
                element
        )


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
    let
        mapFunc =
            draggedOverHelp id position
    in
    case dragged of
        Just element ->
            if not (Element.isGroup element) && isTopLevel then
                Element.concatMap (mapFunc Nothing) editorRoot

            else
                Element.concatMap
                    (mapFunc (Just Element.placeholder))
                    editorRoot

        Nothing ->
            editorRoot


draggedOverHelp : Id -> Position -> Maybe Element -> Element -> List Element
draggedOverHelp containerId position placeholder element =
    if Element.id element == containerId then
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
                            [ Field.label (params.label |> Maybe.withDefault "")
                            , Field.placeholder (params.placeholder |> Maybe.withDefault "")
                            , Field.hint (params.hint |> Maybe.withDefault "")
                            , Field.required params.isRequired
                            , Field.disabled True
                            ]

                    Element.Checkbox ->
                        Field.checkbox
                            [ Field.label (params.label |> Maybe.withDefault "")
                            , Field.hint (params.hint |> Maybe.withDefault "")
                            , Field.disabled True
                            ]

                    Element.IntegerField { min, max } ->
                        Field.int
                            [ Field.label (params.label |> Maybe.withDefault "")
                            , Field.placeholder (params.placeholder |> Maybe.withDefault "")
                            , Field.hint (params.hint |> Maybe.withDefault "")
                            , Field.min min
                            , Field.max max
                            , Field.required params.isRequired
                            , Field.disabled True
                            ]

                    Element.DateField { min, max } ->
                        Field.date
                            [ Field.label (params.label |> Maybe.withDefault "")
                            , Field.hint (params.hint |> Maybe.withDefault "")
                            , Field.min min
                            , Field.max max
                            , Field.required params.isRequired
                            , Field.disabled True
                            ]

                    Element.MonthField { min, max } ->
                        Field.month
                            [ Field.label (params.label |> Maybe.withDefault "")
                            , Field.hint (params.hint |> Maybe.withDefault "")
                            , Field.min min
                            , Field.max max
                            , Field.required params.isRequired
                            , Field.disabled True
                            ]

                    Element.Select options ->
                        Field.select
                            [ Field.label (params.label |> Maybe.withDefault "")
                            , Field.placeholder (params.placeholder |> Maybe.withDefault "")
                            , Field.hint (params.hint |> Maybe.withDefault "")
                            , Field.options (List.map (\( v, l ) -> ( l, Value.string v )) options)
                            , Field.required params.isRequired
                            , Field.disabled True
                            ]

                    Element.Radio options ->
                        Field.radio
                            [ Field.label (params.label |> Maybe.withDefault "")
                            , Field.hint (params.hint |> Maybe.withDefault "")
                            , Field.options (List.map (\( v, l ) -> ( l, Value.string v )) options)
                            , Field.required params.isRequired
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
                Just (Field.group [ Field.label (params.label |> Maybe.withDefault "") ] children)

        RepeatableGroup params ->
            let
                children =
                    List.filterMap elementToPreviewField params.elements
            in
            if List.isEmpty children then
                Nothing

            else
                Just (Field.group [ Field.label (params.label |> Maybe.withDefault "") ] children)

        Review params ->
            Just
                (Field.textarea
                    [ Field.label (params.name |> Maybe.withDefault "")
                    , Field.value (Value.string (params.text |> Maybe.withDefault ""))
                    , Field.disabled True
                    ]
                )

        Help params ->
            Just
                (Field.text
                    [ Field.label (params.button |> Maybe.withDefault "")
                    , Field.value (Value.string (params.text |> Maybe.withDefault ""))
                    , Field.disabled True
                    ]
                )

        Blank _ ->
            Nothing


view : Model -> Html Msg
view model =
    div [ class "editor-layout" ]
        [ div [ class "schema-editor-container" ]
            [ div [ class "schema-editor" ]
                [ section [ class "schema-editor-panes" ]
                    [ div [ class "schema-editor-palete" ]
                        [ addFieldHtml False Element.text
                        , addFieldHtml False Element.checkbox
                        , addFieldHtml False Element.integer
                        , addFieldHtml False Element.date
                        , addFieldHtml False Element.month
                        , addFieldHtml False (Element.select [])
                        , addFieldHtml False (Element.radio [])
                        , addElementHtml False "repeatable-group" Element.repeatableGroup
                        , addElementHtml False "group-addition" Element.group
                        ]
                    , div [ class "schema-editor-tree" ]
                        (if Element.isEmptyGroup model.element then
                            [ placeholderHtml model.element ]

                         else
                            let
                                selected =
                                    model.selected
                            in
                            model.element
                                |> Element.elements
                                |> List.map (elementToHtml True selected)
                        )
                    , sidePane model.selected model.editForm
                    ]
                ]
            ]
        , div [ class "editor-preview-pane" ]
            [ div [ class "tab-nav" ]
                [ button
                    [ classList
                        [ ( "tab-button", True )
                        , ( "active", model.activeTab == PreviewTab )
                        ]
                    , onClick (TabSwitched PreviewTab)
                    ]
                    [ text "Preview" ]
                , button
                    [ classList
                        [ ( "tab-button", True )
                        , ( "active", model.activeTab == JsonTab )
                        ]
                    , onClick (TabSwitched JsonTab)
                    ]
                    [ text "JSON" ]
                ]
            , div [ class "tab-content" ]
                (case model.activeTab of
                    PreviewTab ->
                        let
                            children =
                                Element.elements model.element
                                    |> List.filterMap elementToPreviewField
                        in
                        [ div [ class "tab-pane" ]
                            [ Html.map (always NoOp)
                                (Field.toHtml identity
                                    (Field.group [] children)
                                )
                            ]
                        ]

                    JsonTab ->
                        [ div [ class "tab-pane" ]
                            [ text "JSON content coming soon" ]
                        ]
                )
            ]
        ]


sidePane : Maybe Element -> EditorForm.EditorForm -> Html Msg
sidePane selected editForm =
    case selected of
        Just element ->
            aside [ class "side-pane" ]
                [ h3 [] [ text "Edit Element" ]
                , Html.map FormMsg (EditorForm.view editForm)
                , button [ onClick (ElementRemoved (Element.id element)), class "button" ]
                    [ text "Remove Element" ]
                ]

        Nothing ->
            aside [ class "side-pane" ] [ text "Select an element to edit" ]


addFieldHtml : Bool -> Element -> Html Msg
addFieldHtml disabled element =
    addElementHtml disabled "field-addition" element


addElementHtml : Bool -> String -> Element -> Html Msg
addElementHtml disabled class_ element =
    div
        (class class_
            :: class "editor-add"
            :: (if disabled then
                    [ class "disabled" ]

                else
                    [ Attributes.draggable "true"
                    , on "dragend" <| Decode.succeed DragEnded
                    , on "dragstart" <|
                        Decode.succeed (DragStarted (Add element))
                    ]
               )
        )
        [ div [ class "drag-handle" ] [ icon "gg-layout-grid-small" ]
        , elementIcon element
        , h3 [] [ text (Element.label element) ]
        ]


elementToHtml : Bool -> Maybe Element -> Element -> Html Msg
elementToHtml isTopLevel selected element =
    case element of
        ElementGroup params ->
            div
                (class "element group-element"
                    :: identifier element
                    :: dragAttributes
                        { isTopLevel = isTopLevel
                        , selected = selected
                        , draggable = element
                        , container = element
                        }
                )
                [ div [ class "group" ]
                    (groupHtmlContent selected element params)
                ]

        RepeatableGroup params ->
            div
                (class "element group-element"
                    :: identifier element
                    :: dragAttributes
                        { isTopLevel = isTopLevel
                        , selected = selected
                        , draggable = element
                        , container = element
                        }
                )
                [ div [ class "repeatable-group" ]
                    (groupHtmlContent selected element params)
                ]

        FieldElement _ ->
            fieldHtml
                { selectedNode = selected
                , nodeClass = "field-element"
                , isTopLevel = isTopLevel
                , element = element
                }

        Review _ ->
            fieldHtml
                { selectedNode = selected
                , nodeClass = "review-element"
                , isTopLevel = isTopLevel
                , element = element
                }

        Help _ ->
            fieldHtml
                { selectedNode = selected
                , nodeClass = "help-element"
                , isTopLevel = isTopLevel
                , element = element
                }

        Blank _ ->
            placeholderHtml element


fieldHtml :
    { selectedNode : Maybe Element
    , nodeClass : String
    , isTopLevel : Bool
    , element : Element
    }
    -> Html Msg
fieldHtml { selectedNode, nodeClass, isTopLevel, element } =
    div
        (class "element"
            :: class nodeClass
            :: identifier element
            :: dragAttributes
                { isTopLevel = isTopLevel
                , selected = selectedNode
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
                ]
            ]
        ]


groupHtmlContent :
    Maybe Element
    -> Element
    -> { a | inline : Bool, elements : List Element, isOpen : Bool }
    -> List (Html Msg)
groupHtmlContent selected element { inline, elements, isOpen } =
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
            ]
        , div
            [ class "group-fields"
            , classList [ ( "inline", inline ), ( "stacked", not inline ) ]
            ]
            (if isOpen then
                List.map (elementToHtml False selected) elements

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

        containerId =
            Element.id container

        selectionClass =
            if Maybe.map Element.id selected == Just draggableId then
                class "element-selected"

            else
                class ""
    in
    case Element.drag draggable of
        Drag.Idle ->
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


idToString : Id -> String
idToString elementId =
    Id.toIdentifier Nothing elementId
        |> Maybe.withDefault "unset"
