module Chapters.Fields exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html
import Html.Attributes as Attr
import Task


type ContactFields
    = Name
    | Email


type alias Book book =
    { book | fields : Model }


type Msg
    = TextChanged (Field.Msg ())
    | TextareaChanged (Field.Msg ())
    | EmailChanged (Field.Msg ())
    | UrlChanged (Field.Msg ())
    | FieldWithPatternChanged (Field.Msg ())
    | PasswordChanged (Field.Msg ())
    | AutocompleteTextChanged (Field.Msg ())
    | AutocompleteChanged (Field.Msg ())
    | IntChanged (Field.Msg ())
    | FloatChanged (Field.Msg ())
    | DateChanged (Field.Msg ())
    | DatetimeChanged (Field.Msg ())
    | MonthChanged (Field.Msg ())
    | SelectChanged (Field.Msg ())
    | RadioChanged (Field.Msg ())
    | CheckboxChanged (Field.Msg ())
    | FileChanged (Field.Msg ())
    | GroupChanged (Field.Msg ContactFields)
    | RepeatableChanged (Field.Msg ContactFields)
    | Repeatable2Changed (Field.Msg ContactFields)


type alias Model =
    { text : Field ()
    , textarea : Field ()
    , email : Field ()
    , url : Field ()
    , pattern : Field ()
    , password : Field ()
    , autocompleteText : Field ()
    , autocomplete : Field ()
    , int : Field ()
    , float : Field ()
    , date : Field ()
    , datetime : Field ()
    , month : Field ()
    , select : Field ()
    , radio : Field ()
    , checkbox : Field ()
    , file : Field ()
    , group : Field ContactFields
    , repeatable : Field ContactFields
    , repeatableWithDefaults : Field ContactFields
    }


init : Model
init =
    { text = textField
    , textarea = textareaField
    , email = emailField
    , url = urlField
    , pattern = patternField
    , password = passwordField
    , autocompleteText = autocompleteTextField
    , autocomplete = autocompleteField
    , int = intField
    , float = floatField
    , date = dateField
    , datetime = datetimeField
    , month = monthField
    , select = selectField
    , radio = radioField
    , checkbox = checkboxField
    , file = fileField
    , group = groupField
    , repeatable = repeatableField
    , repeatableWithDefaults = repeatableFieldWithDefaults
    }


update : Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg book =
    let
        model =
            book.fields

        ( newModel, cmd ) =
            case msg of
                TextChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.text
                    in
                    ( { model | text = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                TextareaChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.textarea
                    in
                    ( { model | textarea = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                EmailChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.email
                    in
                    ( { model | email = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                UrlChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.url
                    in
                    ( { model | url = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                FieldWithPatternChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.pattern
                    in
                    ( { model | pattern = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                PasswordChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.password
                    in
                    ( { model | password = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                AutocompleteTextChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.autocompleteText
                    in
                    ( { model | autocompleteText = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                AutocompleteChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.int fieldMsg model.autocomplete
                    in
                    ( { model | autocomplete = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                IntChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.int fieldMsg model.int
                    in
                    ( { model | int = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                FloatChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.float fieldMsg model.float
                    in
                    ( { model | float = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                DateChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.posix fieldMsg model.date
                    in
                    ( { model | date = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                DatetimeChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.posix fieldMsg model.datetime
                    in
                    ( { model | datetime = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                MonthChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.posix fieldMsg model.month
                    in
                    ( { model | month = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                SelectChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.select
                    in
                    ( { model | select = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                RadioChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.radio
                    in
                    ( { model | radio = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                CheckboxChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.bool fieldMsg model.checkbox
                    in
                    ( { model | checkbox = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                FileChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.file fieldMsg model.file
                    in
                    ( { model | file = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                GroupChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate contactParser fieldMsg model.group
                    in
                    ( { model | group = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                RepeatableChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate (Parse.list contactParser) fieldMsg model.repeatable
                    in
                    ( { model | repeatable = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                Repeatable2Changed fieldMsg ->
                    let
                        ( updatedField, result ) =
                            model.repeatableWithDefaults
                                |> Parse.parseUpdate (Parse.list contactParser) fieldMsg
                    in
                    ( { model | repeatableWithDefaults = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )
    in
    ( { book | fields = newModel }, cmd )


chapter : Chapter (Book book)
chapter =
    Chapter.chapter "Form building blocks"
        |> Chapter.withStatefulComponentList
            [ ( "Text"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.text
                            |> Field.toHtml TextChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Textarea"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.textarea
                            |> Field.toHtml TextareaChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Field with Pattern"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.pattern
                            |> Field.toHtml FieldWithPatternChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Email"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.email
                            |> Field.toHtml EmailChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Url"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.url
                            |> Field.toHtml UrlChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Password"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.password
                            |> Field.toHtml PasswordChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Text with suggestions"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.autocompleteText
                            |> Field.toHtml AutocompleteTextChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Autocomplete"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.autocomplete
                            |> Field.toHtml AutocompleteChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Int"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.int
                            |> Field.toHtml IntChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Float"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.float
                            |> Field.toHtml FloatChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Date"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.date
                            |> Field.toHtml DateChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Datetime"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.datetime
                            |> Field.toHtml DatetimeChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Month"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.month
                            |> Field.toHtml MonthChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Select"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.select
                            |> Field.toHtml SelectChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Radio"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.radio
                            |> Field.toHtml RadioChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Checkbox"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.checkbox
                            |> Field.toHtml CheckboxChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "File"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.file
                            |> Field.toHtml FileChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Group"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.group
                            |> Field.toHtml GroupChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Repeatable"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.repeatable
                            |> Field.toHtml RepeatableChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Repeatable With Defaults"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fields.repeatableWithDefaults
                            |> Field.toHtml Repeatable2Changed
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            ]
        |> Chapter.render markdownContent



-- MARKDOWN CONTENT COMPONENTS


markdownContent : String
markdownContent =
    """

The building block is `Field`. A Field can be a single input field, a group of
Fields, or a group of repeatable Fields. It has a similar API to Elm Html, but
since it tracks state, it should be part of the model.


### Text

Basic text input. Use `Field.placeholder` for user guidance, `Field.hint`
for help text, and `Field.required` to enforce validation.

```elm
textField : Field ()
textField =
    Field.text
        [ Field.label "Text Field"
        , Field.placeholder "Enter any text"
        , Field.hint "This field accepts any text input"
        , Field.required True
        ]
```

<component with-label="Text"/>

Parsed using `Parse.string`.


### Textarea

Multi-line text input. Use `Field.autogrow` to automatically expand the height
as content grows.

```elm
textareaField : Field ()
textareaField =
    Field.textarea
        [ Field.label "Textarea Field"
        , Field.placeholder "Enter multiple lines of text"
        , Field.autogrow True
        , Field.required True
        ]
```

<component with-label="Textarea"/>

Parsed using `Parse.string`.


### Text with Pattern

Text input with pattern validation that formats and validates input according to a mask.
Only characters matching the pattern will be allowed. The input will be formatted 
automatically and validated on blur.

```elm
patternField : Field ()
patternField =
    Field.text
        [ Field.label "Credit Card"
        , Field.placeholder "Enter your credit card"
        , Field.pattern "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
        , Field.required True
        ]
```

<component with-label="Field with Pattern"/>

Mask tokens:

- `{d}` - matches digits
- `{D}` - matches non-digits  
- `{w}` - matches word characters (alphanumeric + underscore)
- `{W}` - matches non-word characters
- Any other character is treated as a literal

Parsed using `Parse.string`.


### Email

Email validation with proper input type and built-in validation.

```elm
emailField : Field ()
emailField =
    Field.email
        [ Field.label "Email Field"
        , Field.placeholder "your@email.com"
        , Field.required True
        ]
```

<component with-label="Email"/>

Parsed using `Parse.string`.


### URL

URL validation with proper input type and built-in validation.

```elm
urlField : Field ()
urlField =
    Field.url
        [ Field.label "URL Field"
        , Field.placeholder "https://example.com"
        , Field.required True
        ]
```

<component with-label="Url"/>

Parsed using `Parse.url` to return a `Url.Url` type.


### Password

Masked input for sensitive data with security features.

```elm
passwordField : Field ()
passwordField =
    Field.password
        [ Field.label "Password Field"
        , Field.placeholder "Enter password"
        , Field.required True
        ]
```

<component with-label="Password"/>

Parsed using `Parse.string`.


### Text with Suggestions

Text input that shows suggestions while allowing free text entry. Use
`Field.stringOptions` to provide a list of suggestions.

```elm
autocompleteTextField : Field ()
autocompleteTextField =
    Field.text
        [ Field.label "Text with suggestions"
        , Field.placeholder "Type to see suggestions"
        , Field.stringOptions [ "Apple", "Banana", "Cherry", "Date", "Elderberry", "Fig" ]
        , Field.required True
        ]
```

<component with-label="Text with suggestions"/>

Parsed using `Parse.string`.


### Strict Autocomplete

Strict autocomplete that only allows selection from predefined options.
Use `Field.strictAutocomplete` instead of `Field.text`.


```elm
    Field.strictAutocomplete
        [ Field.label "Strict Autocomplete"
        , Field.placeholder "Choose from options"
        , Field.options
            [ ( "Apple", Value.int 1 )
            , ( "Banana", Value.int 2 )
            , ( "Cherry", Value.int 3 )
            , ( "Date", Value.int 4 )
            ]
        , Field.required True
        ]
```

<component with-label="Autocomplete"/>

Parsed using `Parse.int`.

For all fields accepting options, either `Field.stringOptions` or
`Field.options` can be used.


### Integer

Integer input with number validation. Use `Field.min` and `Field.max` to set
validation boundaries and `Field.step` to control increment steps for numeric
values.

```elm
intField : Field ()
intField =
    Field.int
        [ Field.label "Integer Field"
        , Field.placeholder "Enter a whole number"
        , Field.min (Value.int 0)
        , Field.max (Value.int 100)
        , Field.step (Value.int 5)
        , Field.required True
        ]
```

<component with-label="Int"/>

Parsed using `Parse.int`.


### Float

Decimal number input with precision control and validation. Use `Value.float`
for floating-point boundaries.

```elm
floatField : Field ()
floatField =
    Field.float
        [ Field.label "Float Field"
        , Field.placeholder "Enter a decimal number"
        , Field.min (Value.float 0.0)
        , Field.max (Value.float 10.0)
        , Field.step (Value.float 0.1)
        , Field.required True
        ]
```

<component with-label="Float"/>

Parsed using `Parse.float`.


### Date

Date picker with built-in validation and date formatting.

```elm
dateField : Field ()
dateField =
    Field.date
        [ Field.label "Date Field"
        , Field.required True
        ]
```

<component with-label="Date"/>

Parsed using `Parse.posix`.


### Datetime

Datetime picker with built-in validation for date and time selection.

```elm
datetimeField : Field ()
datetimeField =
    Field.datetime
        [ Field.label "Datetime Field"
        , Field.required True
        ]
```

<component with-label="Datetime"/>

Parsed using `Parse.posix`.


### Month

Month/year selection with proper validation.

```elm
monthField : Field ()
monthField =
    Field.month
        [ Field.label "Month Field"
        , Field.required True
        ]
```

<component with-label="Month"/>

Parsed using `Parse.posix`.


### Select

Dropdown selection from predefined options. Use `Field.options` with tuples of
display text and `Value.string` for the underlying values.

```elm
selectField : Field ()
selectField =
    Field.select
        [ Field.label "Select Field"
        , Field.required True
        , Field.options
            [ ( "Small", Value.string "small" )
            , ( "Medium", Value.string "medium" )
            , ( "Large", Value.string "large" )
            , ( "Extra Large", Value.string "xlarge" )
            ]
        ]
```

<component with-label="Select"/>

Parsed using `Parse.string`.


### Radio

Single selection from visible options with proper grouping.

```elm
radioField : Field ()
radioField =
    Field.radio
        [ Field.label "Radio Field"
        , Field.required True
        , Field.options
            [ ( "Red", Value.string "red" )
            , ( "Green", Value.string "green" )
            , ( "Blue", Value.string "blue" )
            ]
        ]
```

<component with-label="Radio"/>

Parsed using `Parse.string`.


### Checkbox

Boolean input for yes/no values with proper accessibility.

```elm
checkboxField : Field ()
checkboxField =
    Field.checkbox
        [ Field.label "Checkbox Field"
        ]
```

<component with-label="Checkbox"/>

Parsed using `Parse.bool`.


### File

A file upload input field with drag and drop support. Files can be dropped onto
the input area or selected via the native file picker. The `min` and `max`
attributes can be used to validate file size in bytes.

```elm
fileField : Field ()
fileField =
    Field.file
        [ Field.label "File Upload"
        , Field.placeholder "Choose a file or drag and drop"
        , Field.hint "Maximum file size: 5MB"
        , Field.max (Value.int 5242880)
        , Field.required True
        ]
```

<component with-label="File"/>

Parsed using `Parse.file`.


## Groupping Fields


### Group

Group multiple related fields together with shared validation and styling. Use
`Field.label` to set the legend text for the group.


```elm
type ContactFields
    = Name
    | Email

groupField : Field ContactFields
groupField =
    Field.group
        [ Field.label "Contact Information"
        ]
        [ Field.text
            [ Field.label "Name"
            , Field.placeholder "Enter your name"
            , Field.required True
            , Field.identifier Name
            ]
        , Field.email
            [ Field.label "Email"
            , Field.placeholder "your@email.com"
            , Field.required True
            , Field.identifier Email
            ]
        ]
```

<component with-label="Group"/>


Parsed using `contactParser`:


```
contactParser : Parse.Parser ContactFields { name : String, email : String }
contactParser =
    Parse.map2 (\\name email -> { name = name, email = email })
        (Parse.field Name Parse.string)
        (Parse.field Email Parse.string)
```


### Repeatable

Create repeatable field groups that allow users to dynamically add and remove
field instances. Use `Field.repeatableMin` and `Field.repeatableMax` to set
limits, and `Field.copies` to customize button text.


```elm
repeatableField : Field ContactFields
repeatableField =
    Field.repeatable
        [ Field.label "Contact Information"
        , Field.name "contacts"
        , Field.repeatableMin 1
        , Field.repeatableMax 5
        , Field.copies
            { addFieldsButton = "Add Contact"
            , removeFieldsButton = "Remove"
            }
        ]
        (Field.group
            [ Field.class "inline-fields" ]
            [ Field.text
                [ Field.label "Name"
                , Field.placeholder "Enter contact name"
                , Field.required True
                , Field.identifier Name
                , Field.name "name"
                ]
            , Field.email
                [ Field.label "Email"
                , Field.placeholder "contact@email.com"
                , Field.required True
                , Field.identifier Email
                , Field.name "email"
                ]
            ]
        )
        []


```

<component with-label="Repeatable"/>

Parsed using `Parse.list contactParser`.


### Repeatable With Defaults

Repeatable fields can be initialized with default values by passing a list of
functions that update field attributes.

The minimum number of fields enforces a lower bound - fields cannot be removed
below this count. If more defaults are provided than the minimum, extra fields
are added but remain removable.

```elm
repeatableFieldWithDefaults : Field ContactFields
repeatableFieldWithDefaults =
    Field.repeatable
        [ Field.label "Contact Information"
        , Field.name "contacts"
        , Field.repeatableMin 2   
        , Field.repeatableMax 5
        , Field.copies
            { addFieldsButton = "Add Contact"
            , removeFieldsButton = "Remove"
            }
        ]
        (Field.group
            [ Field.class "inline-fields" ]
            [ Field.text
                [ Field.label "Name"
                , Field.placeholder "Enter contact name"
                , Field.required True
                , Field.identifier Name
                ]
            , Field.email
                [ Field.label "Email"
                , Field.placeholder "contact@email.com"
                , Field.required True
                , Field.identifier Email
                ]
            ]
        )
        [ -- A list of functions that will be used to
          -- initialize each of the default repeatable
          -- fields, only a number of elements up to
          -- repeatableMax number will be considered, the
          -- rest will be dropped.
          Field.updateWithId Name
            (Field.value
                (Value.string "Brian Eno")
            )
        , Field.updateWithId Name
            (Field.value
                (Value.string "Faust")
            )
        , Field.updateWithId Name
            (Field.value
                (Value.string "Neu!")
            )
        ]
```

<component with-label="Repeatable With Defaults"/>

Parsed using `Parse.list contactParser`.
"""


textField : Field ()
textField =
    Field.text
        [ Field.label "Text Field"
        , Field.placeholder "Enter any text"
        , Field.hint "This field accepts any text input"
        , Field.required True
        ]


textareaField : Field ()
textareaField =
    Field.textarea
        [ Field.label "Textarea Field"
        , Field.placeholder "Enter multiple lines of text"
        , Field.autogrow True
        , Field.required True
        ]


patternField : Field ()
patternField =
    Field.text
        [ Field.label "Credit Card"
        , Field.placeholder "Enter your credit card"
        , Field.pattern "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
        , Field.required True
        ]


emailField : Field ()
emailField =
    Field.email
        [ Field.label "Email Field"
        , Field.placeholder "your@email.com"
        , Field.required True
        ]


urlField : Field ()
urlField =
    Field.url
        [ Field.label "URL Field"
        , Field.placeholder "https://example.com"
        , Field.required True
        ]


passwordField : Field ()
passwordField =
    Field.password
        [ Field.label "Password Field"
        , Field.placeholder "Enter password"
        , Field.required True
        ]


autocompleteTextField : Field ()
autocompleteTextField =
    Field.text
        [ Field.label "Text with suggestions"
        , Field.placeholder "Type to see suggestions"
        , Field.stringOptions [ "Apple", "Banana", "Cherry", "Date", "Elderberry", "Fig" ]
        , Field.required True
        ]


autocompleteField : Field ()
autocompleteField =
    Field.strictAutocomplete
        [ Field.label "Strict Autocomplete"
        , Field.placeholder "Choose from options"
        , Field.options
            [ ( "Apple", Value.int 1 )
            , ( "Banana", Value.int 2 )
            , ( "Cherry", Value.int 3 )
            , ( "Date", Value.int 4 )
            ]
        , Field.required True
        ]


intField : Field ()
intField =
    Field.int
        [ Field.label "Integer Field"
        , Field.placeholder "Enter a whole number"
        , Field.min (Value.int 0)
        , Field.max (Value.int 100)
        , Field.step (Value.int 5)
        , Field.required True
        ]


floatField : Field ()
floatField =
    Field.float
        [ Field.label "Float Field"
        , Field.placeholder "Enter a decimal number"
        , Field.min (Value.float 0.0)
        , Field.max (Value.float 10.0)
        , Field.step (Value.float 0.1)
        , Field.required True
        ]


dateField : Field ()
dateField =
    Field.date
        [ Field.label "Date Field"
        , Field.required True
        ]


datetimeField : Field ()
datetimeField =
    Field.datetime
        [ Field.label "Datetime Field"
        , Field.required True
        ]


monthField : Field ()
monthField =
    Field.month
        [ Field.label "Month Fiend"
        , Field.required True
        ]


selectField : Field ()
selectField =
    Field.select
        [ Field.label "Select Field"
        , Field.required True
        , Field.options
            [ ( "Small", Value.string "small" )
            , ( "Medium", Value.string "medium" )
            , ( "Large", Value.string "large" )
            , ( "Extra Large", Value.string "xlarge" )
            ]
        ]


radioField : Field ()
radioField =
    Field.radio
        [ Field.label "Radio Field"
        , Field.required True
        , Field.options
            [ ( "Red", Value.string "red" )
            , ( "Green", Value.string "green" )
            , ( "Blue", Value.string "blue" )
            ]
        ]


checkboxField : Field ()
checkboxField =
    Field.checkbox
        [ Field.label "Checkbox Field"
        ]


fileField : Field ()
fileField =
    Field.file
        [ Field.label "File Upload"
        , Field.placeholder "Choose a file or drag and drop"
        , Field.hint "Maximum file size: 5MB"
        , Field.max (Value.int 5242880)
        , Field.required True
        ]


groupField : Field ContactFields
groupField =
    Field.group
        [ Field.label "Contact Information" ]
        [ Field.text
            [ Field.label "Name"
            , Field.placeholder "Enter your name"
            , Field.required True
            , Field.identifier Name
            ]
        , Field.email
            [ Field.label "Email"
            , Field.placeholder "your@email.com"
            , Field.required True
            , Field.identifier Email
            ]
        ]


repeatableField : Field ContactFields
repeatableField =
    Field.repeatable
        [ Field.label "Contact Information"
        , Field.name "contacts"
        , Field.repeatableMin 1
        , Field.repeatableMax 5
        , Field.copies
            { addFieldsButton = "Add Contact"
            , removeFieldsButton = "Remove"
            }
        ]
        (Field.group
            [ Field.class "inline-fields" ]
            [ Field.text
                [ Field.label "Name"
                , Field.placeholder "Enter contact name"
                , Field.required True
                , Field.identifier Name
                ]
            , Field.email
                [ Field.label "Email"
                , Field.placeholder "contact@email.com"
                , Field.required True
                , Field.identifier Email
                ]
            ]
        )
        []


repeatableFieldWithDefaults : Field ContactFields
repeatableFieldWithDefaults =
    Field.repeatable
        [ Field.label "Contact Information"
        , Field.name "contacts"
        , Field.repeatableMin 2
        , Field.repeatableMax 5
        , Field.copies
            { addFieldsButton = "Add Contact"
            , removeFieldsButton = "Remove"
            }
        ]
        (Field.group
            [ Field.class "inline-fields" ]
            [ Field.text
                [ Field.label "Name"
                , Field.placeholder "Enter contact name"
                , Field.required True
                , Field.identifier Name
                ]
            , Field.email
                [ Field.label "Email"
                , Field.placeholder "contact@email.com"
                , Field.required True
                , Field.identifier Email
                ]
            ]
        )
        [ Field.updateWithId Name (Field.stringValue "Brian Eno")
        , Field.updateWithId Name (Field.stringValue "Faust")
        , Field.updateWithId Name (Field.stringValue "Neu!")
        ]


contactParser : Parse.Parser ContactFields { name : String, email : String }
contactParser =
    Parse.map2 (\name email -> { name = name, email = email })
        (Parse.field Name Parse.string)
        (Parse.field Email Parse.string)
