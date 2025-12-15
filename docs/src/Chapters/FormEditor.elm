module Chapters.FormEditor exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Editor as Editor
import Html exposing (Html)
import Html.Attributes as Attr


type alias Model =
    { editor : Editor.Editor String }


type Msg
    = EditorMsg (Editor.Msg String)


init : Model
init =
    { editor = Editor.init }


update : Msg -> Model -> ( Model, Cmd (ElmBook.Msg state) )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            let
                ( editor, _ ) =
                    Editor.update editorMsg model.editor
            in
            ( { model | editor = editor }, Cmd.none )


chapter : Chapter { x | formEditor : Model }
chapter =
    Chapter.chapter "Form Editor"
        |> Chapter.withStatefulComponentList
            [ ( "Editor"
              , \book ->
                    viewEditor book.formEditor.editor
                        |> Html.map
                            (Actions.updateStateWithCmdWith
                                (\msg state ->
                                    update (EditorMsg msg) state.formEditor
                                        |> Tuple.mapFirst
                                            (\formEditor ->
                                                { state | formEditor = formEditor }
                                            )
                                )
                            )
              )
            ]
        |> Chapter.render markdownContent


viewEditor : Editor.Editor String -> Html (Editor.Msg String)
viewEditor editor =
    Html.div
        [ Attr.style "border" "1px solid #ddd"
        , Attr.style "border-radius" "4px"
        , Attr.style "min-height" "500px"
        ]
        [ Editor.view editor ]


markdownContent : String
markdownContent =
    """
# Form Editor

The Form Editor is a drag-and-drop interface for building forms visually. It allows you to:

- Drag field types from a palette into a tree structure
- Rearrange fields by dragging them to new positions
- Configure field properties through a property panel
- Organize fields into groups and repeatable sections


<component with-label="Editor"/>


## Basic Usage

To use the Form Editor in your application, you need to:

1. Add the Editor to your model
2. Handle Editor messages in your update function
3. Render the Editor view


### Step 1: Add to Model

```elm
import FormToolkit.Editor as Editor

type alias Model =
    { editor : Editor.Editor FieldId
    , otherFields : ...
    }

init : Model
init =
    { editor = Editor.init
    , otherFields = ...
    }
```


### Step 2: Handle Messages

```elm
type Msg
    = EditorMsg (Editor.Msg FieldId)
    | ...

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            let
                ( editor, cmd ) =
                    Editor.update editorMsg model.editor
            in
            ( { model | editor = editor }
            , Cmd.map EditorMsg cmd
            )

        ...
```


### Step 3: Render the View

```elm
view : Model -> Html Msg
view model =
    Html.div []
        [ Html.map EditorMsg (Editor.view model.editor)
        ]
```


## Features

### Palette

The left panel contains draggable field types:

- **Text Field** - Single-line text input
- **Text Area** - Multi-line text input
- **Number** - Numeric input with optional min/max
- **Date** - Date picker
- **Select** - Dropdown selection
- **Radio** - Radio button group
- **Checkbox** - Single checkbox
- **Group** - Container for organizing fields
- **Repeatable** - Dynamic list of fields


### Tree View

The center panel shows your form structure as a tree. You can:

- Click on fields to select and edit them
- Drag fields to reorder them
- Drag fields into groups
- Expand/collapse groups by clicking the chevron icon


### Property Panel

The right panel shows properties for the selected field:

- **Name** - Field identifier (used in form data)
- **Label** - Display label shown to users
- **Placeholder** - Placeholder text for inputs
- **Hint** - Help text shown below the field
- **Required** - Whether the field must be filled


## Drag and Drop Behavior

The editor uses HTML5 drag and drop with visual feedback:

1. **Enable drag**: Click and hold the drag handle (grid icon) on any field
2. **Drag**: Move the field over the tree
3. **Drop target**: A placeholder shows where the field will be inserted
4. **Drop**: Release to insert the field


### Position Detection

When dragging over a field, the drop position is calculated based on:

- Mouse position relative to the target element
- Whether the target is a group (can contain children)
- Whether dropping at the top level


## Working with the Field

Once you've built a form in the editor, you can access the underlying `Field`:

```elm
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse

getFormDefinition : Editor.Editor id -> Field id
getFormDefinition editor =
    editor.field

parseEditorOutput : Editor.Editor id -> Result String YourDataType
parseEditorOutput editor =
    Parse.run yourParser editor.field
```


## Styling

The editor comes with default CSS classes that you can customize:

- `.schema-editor-container` - Main container
- `.schema-editor-palette` - Left palette panel
- `.schema-editor-tree` - Center tree view
- `.side-pane` - Right property panel
- `.element` - Individual field in tree
- `.element-selected` - Selected field
- `.drag-handle` - Drag handle icon
- `.drag-enabled` - Field ready to drag
- `.drag-dragged` - Field being dragged


## Example: Custom Field Identifiers

Use a custom type for better type safety:

```elm
type MyFieldId
    = FirstNameField
    | LastNameField
    | EmailField
    | AddressGroup
    | StreetField
    | CityField

type alias Model =
    { editor : Editor.Editor MyFieldId }

init : Model
init =
    { editor = Editor.init }
```


## Limitations

Current limitations (planned for future releases):

- Property forms are not yet fully implemented for all field types
- Drop logic for inserting fields is not yet complete
- Field removal needs to be implemented
- No undo/redo functionality yet
- No JSON serialization yet


## Next Steps

- See the [Field Reference](../Fields) for details on available field types
- Check out [Parsing](../ParsingCookbook) to learn how to extract data from forms
- Learn about [Customization](../Customization) to style the editor
"""
