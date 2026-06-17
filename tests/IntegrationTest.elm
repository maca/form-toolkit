module IntegrationTest exposing (suite)

import Expect
import FormToolkit.Error as Error
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html.Attributes as Attrs exposing (for, name, required)
import Json.Encode
import Support.ExampleInputs exposing (..)
import Support.Interaction as Interaction exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, containing, id, tag, text)
import Time


suite : Test
suite =
    describe "Integration Tests"
        [ stringFieldTests
        , strictAutocompleteFieldTests
        , strictAutocompleteJsonTests
        , checkboxFieldTests
        , selectFieldTests
        , radioFieldTests
        , dateFieldTests
        , datetimeFieldTests
        , repeatableFieldsTests
        , validationFocusBlurTests
        , validationTests
        ]


stringFieldTests : Test
stringFieldTests =
    describe "string field" <|
        [ test "label and hint" <|
            \_ ->
                stringInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ tag "input" ]
                            >> Query.has
                                [ attribute
                                    (Attrs.attribute "aria-describedby" "string-field-hint")
                                , attribute (Attrs.placeholder "String value")
                                ]
                        , Query.find [ tag "label" ]
                            >> Query.has
                                [ text "Enter your string"
                                , id "string-field-label"
                                , attribute (for "string-field")
                                ]
                        , Query.find [ id "string-field-hint" ]
                            >> Query.has [ text "Must be a string" ]
                        ]
        , test "class" <|
            \_ ->
                stringInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has
                            [ class "field"
                            , class "styled-string-field"
                            ]
                        ]
        , test "validation feedback" <|
            \_ ->
                let
                    { field } =
                        Interaction.init Parse.string blankInput
                            |> blur "blank-field"
                in
                field
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ class "required" ]
                        , Query.has
                            [ containing
                                [ tag "input"
                                , attribute (Attrs.attribute "aria-invalid" "true")
                                ]
                            ]
                        , Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be provided" ] ]
                        ]
        , test "datalist options" <|
            \_ ->
                stringInputWithOptions
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has
                            [ attribute (Attrs.attribute "list" "string-field-datalist")
                            , attribute (Attrs.attribute "autocomplete" "on")
                            ]
                        , Query.find [ tag "datalist" ]
                            >> Expect.all
                                [ Query.has
                                    [ id "string-field-datalist"
                                    , attribute (Attrs.attribute "role" "listbox")
                                    ]
                                , Query.has
                                    [ containing
                                        [ tag "option"
                                        , attribute (Attrs.attribute "value" "red")
                                        ]
                                    ]
                                , Query.has
                                    [ containing
                                        [ tag "option"
                                        , attribute (Attrs.attribute "value" "green")
                                        ]
                                    ]
                                , Query.has
                                    [ containing
                                        [ tag "option"
                                        , attribute (Attrs.attribute "value" "blue")
                                        ]
                                    ]
                                ]
                        ]
        , test "text field with options parses to int value when matching option" <|
            \_ ->
                let
                    textFieldWithOptions =
                        Field.text
                            [ Field.name "color"
                            , Field.options
                                [ ( "Red", Value.int 1 )
                                , ( "Green", Value.int 2 )
                                , ( "Blue", Value.int 3 )
                                ]
                            ]
                in
                Interaction.init Parse.int textFieldWithOptions
                    |> fillInput "color" "Green"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ tag "input", attribute (Attrs.name "color") ]
                            >> Query.has [ attribute (Attrs.value "Green") ]
                        , .result >> Expect.equal (Ok 2)
                        ]
        , test "text field with options fails to parse int when value doesn't match option" <|
            \_ ->
                let
                    textFieldWithOptions =
                        Field.text
                            [ Field.name "color"
                            , Field.options
                                [ ( "Red", Value.int 1 )
                                , ( "Green", Value.int 2 )
                                , ( "Blue", Value.int 3 )
                                ]
                            ]
                in
                Interaction.init Parse.int textFieldWithOptions
                    |> fillInput "color" "Purple"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ tag "input", attribute (Attrs.name "color") ]
                            >> Query.has [ attribute (Attrs.value "Purple") ]
                        , .result >> Expect.err
                        ]
        , test "text field with options parses to string value when not matching option" <|
            \_ ->
                let
                    textFieldWithOptions =
                        Field.text
                            [ Field.name "color"
                            , Field.options
                                [ ( "Red", Value.int 1 )
                                , ( "Green", Value.int 2 )
                                , ( "Blue", Value.int 3 )
                                ]
                            ]
                in
                Interaction.init Parse.string textFieldWithOptions
                    |> fillInput "color" "Purple"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ tag "input", attribute (Attrs.name "color") ]
                            >> Query.has [ attribute (Attrs.value "Purple") ]
                        , .result >> Expect.equal (Ok "Purple")
                        ]
        , test "name attribute renders dasherized class and non-dasherized html name" <|
            \_ ->
                let
                    fieldWithName =
                        Field.text
                            [ Field.name "My Field Name"
                            , Field.label "Test Field"
                            ]
                in
                fieldWithName
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ class "my-field-name" ]
                        , Query.find [ tag "input" ]
                            >> Query.has [ attribute (Attrs.name "My Field Name") ]
                        ]
        ]


strictAutocompleteFieldTests : Test
strictAutocompleteFieldTests =
    let
        autocompleteField =
            Field.strictAutocomplete
                [ Field.label "Language"
                , Field.name "language"
                , Field.options
                    [ ( "Español", Value.int 1 )
                    , ( "English", Value.int 2 )
                    , ( "Deutsch", Value.int 3 )
                    ]
                ]

        stringAutocompleteField =
            Field.strictAutocomplete
                [ Field.label "Country"
                , Field.name "country"
                , Field.stringOptions [ "España", "England", "Deutschland" ]
                ]
    in
    describe "strict autocomplete" <|
        [ test "sets value" <|
            \_ ->
                let
                    { result } =
                        Field.strictAutocomplete
                            [ Field.name "language"
                            , Field.stringOptions [ "Español", "English", "Deutsch" ]
                            ]
                            |> Interaction.init Parse.string
                            |> fillInput "language" "Español"
                in
                Expect.equal result (Ok "Español")
        , test "allows typing incomplete name and keeps value in HTML attribute but parsing fails" <|
            \_ ->
                Interaction.init Parse.int autocompleteField
                    |> fillInput "language" "Esp"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ tag "input", attribute (Attrs.name "language") ]
                            >> Query.has [ attribute (Attrs.value "Esp") ]
                        , .result >> Expect.equal (Err (Error.InvalidValue Nothing))
                        ]
        , test "correctly parses when entering a complete matching option" <|
            \_ ->
                Interaction.init Parse.int autocompleteField
                    |> fillInput "language" "English"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ tag "input", attribute (Attrs.name "language") ]
                            >> Query.has [ attribute (Attrs.value "English") ]
                        , .result >> Expect.equal (Ok 2)
                        ]
        , test "correctly parses string value when using stringOptions" <|
            \_ ->
                Interaction.init Parse.string stringAutocompleteField
                    |> fillInput "country" "England"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ tag "input", attribute (Attrs.name "country") ]
                            >> Query.has [ attribute (Attrs.value "England") ]
                        , .result >> Expect.equal (Ok "England")
                        ]
        , test "Parse.stringLenient succeeds when text doesn't match any option" <|
            \_ ->
                Interaction.init Parse.stringLenient stringAutocompleteField
                    |> fillInput "country" "France"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ tag "input", attribute (Attrs.name "country") ]
                            >> Query.has [ attribute (Attrs.value "France") ]
                        , .result >> Expect.equal (Ok "France")
                        ]
        , test "Parse.stringLenient also works when text matches option" <|
            \_ ->
                Interaction.init Parse.stringLenient stringAutocompleteField
                    |> fillInput "country" "England"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ tag "input", attribute (Attrs.name "country") ]
                            >> Query.has [ attribute (Attrs.value "England") ]
                        , .result >> Expect.equal (Ok "England")
                        ]
        ]


strictAutocompleteJsonTests : Test
strictAutocompleteJsonTests =
    describe "strict autocomplete with Parse.json" <|
        [ test "encodes int value from options, not string label" <|
            \_ ->
                let
                    fruitField =
                        Field.strictAutocomplete
                            [ Field.label "Fruit"
                            , Field.name "fruit"
                            , Field.options
                                [ ( "Apple", Value.int 1 )
                                , ( "Banana", Value.int 2 )
                                , ( "Cherry", Value.int 3 )
                                , ( "Date", Value.int 4 )
                                ]
                            ]

                    { result } =
                        Interaction.init Parse.json fruitField
                            |> fillInput "fruit" "Cherry"
                in
                result
                    |> Result.map (Json.Encode.encode 0)
                    |> Expect.equal (Ok "{\"fruit\":3}")
        , test "complex form with text, checkbox, select with options (int), select with stringOptions, and strictAutocomplete" <|
            \_ ->
                let
                    formField =
                        Field.group []
                            [ Field.text
                                [ Field.label "Name"
                                , Field.name "name"
                                ]
                            , Field.checkbox
                                [ Field.label "Subscribe"
                                , Field.name "subscribe"
                                , Field.value (Value.bool True)
                                ]
                            , Field.select
                                [ Field.label "Size"
                                , Field.name "size"
                                , Field.options
                                    [ ( "Small", Value.int 10 )
                                    , ( "Medium", Value.int 20 )
                                    , ( "Large", Value.int 30 )
                                    ]
                                ]
                            , Field.select
                                [ Field.label "Color"
                                , Field.name "color"
                                , Field.stringOptions [ "Red", "Green", "Blue" ]
                                ]
                            , Field.strictAutocomplete
                                [ Field.label "Fruit"
                                , Field.name "fruit"
                                , Field.options
                                    [ ( "Apple", Value.int 1 )
                                    , ( "Banana", Value.int 2 )
                                    , ( "Cherry", Value.int 3 )
                                    , ( "Date", Value.int 4 )
                                    ]
                                ]
                            ]

                    { field } =
                        Interaction.init Parse.json formField
                            |> fillInput "name" "Alice"
                            |> select "size" "Medium"
                            |> select "color" "Blue"
                            |> fillInput "fruit" "Banana"

                    result =
                        Parse.parse Parse.json field
                in
                result
                    |> Result.map (Json.Encode.encode 0)
                    |> Expect.equal (Ok "{\"name\":\"Alice\",\"subscribe\":true,\"size\":20,\"color\":\"Blue\",\"fruit\":2}")
        ]


checkboxFieldTests : Test
checkboxFieldTests =
    describe "checkbox" <|
        [ test "label references input" <|
            \_ ->
                checkboxInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ tag "input" ]
                            >> Query.has
                                [ attribute
                                    (Attrs.attribute "aria-describedby" "checkbox-hint")
                                ]
                        , Query.find [ tag "label" ]
                            >> Query.has
                                [ text "Accept"
                                , id "checkbox-label"
                                , attribute (for "checkbox")
                                ]
                        , Query.find [ id "checkbox-hint" ]
                            >> Query.has [ text "You have to check the box" ]
                        ]
        ]


selectFieldTests : Test
selectFieldTests =
    describe "select" <|
        [ test "label references input" <|
            \_ ->
                selectInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ tag "label" ]
                            >> Query.has [ text "Language" ]
                        , Query.find [ tag "label" ]
                            >> Query.has [ attribute (for "select") ]
                        , Query.find [ tag "select" ]
                            >> Query.has
                                [ attribute (Attrs.attribute "aria-labelledby" "select-label") ]
                        , Query.find [ tag "select" ]
                            >> Query.has
                                [ attribute (Attrs.attribute "aria-describedby" "select-hint") ]
                        , Query.find [ tag "select" ]
                            >> Query.hasNot
                                [ attribute (Attrs.attribute "aria-invalid" "true") ]
                        , Query.has [ class "required" ]
                        ]
        , test "options" <|
            \_ ->
                selectInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ tag "option", containing [ text "Español" ] ]
                            >> Query.has [ tag "option" ]
                        , Query.find [ tag "option", containing [ text "English" ] ]
                            >> Query.has [ tag "option" ]
                        , Query.find [ tag "option", containing [ text "Deutsch" ] ]
                            >> Query.has [ tag "option" ]
                        ]
        , test "has errors if no options are provided" <|
            \_ ->
                Field.select []
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Query.find [ class "errors" ]
                    |> Query.has [ containing [ text "No options have been provided" ] ]
        , test "shows validation feedback" <|
            \_ ->
                let
                    { field } =
                        Interaction.init Parse.string selectInput
                            |> blur "select"
                in
                field
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ class "required" ]
                        , Query.find [ tag "select" ]
                            >> Query.has
                                [ attribute (Attrs.attribute "aria-invalid" "true") ]
                        , Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be provided" ] ]
                        ]
        ]


radioFieldTests : Test
radioFieldTests =
    describe "radio" <|
        [ test "has labeled radiogroup" <|
            \_ ->
                radioInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find
                            [ tag "label"
                            , attribute (Attrs.for "radio-inputs")
                            , id "radio-inputs-label"
                            ]
                            >> Query.has [ text "Radio inputs" ]
                        , Query.find [ attribute (Attrs.attribute "role" "radiogroup") ]
                            >> Query.has
                                [ attribute
                                    (Attrs.attribute "aria-labelledby" "radio-inputs-label")
                                , attribute
                                    (Attrs.attribute "aria-describedby" "radio-inputs-hint")
                                ]
                        , Query.find [ id "radio-inputs-hint" ]
                            >> Query.has [ text "Turn the light on or off" ]
                        ]
        , test "has options described by hint" <|
            \_ ->
                radioInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ id "radio-inputs-hint" ]
                            >> Query.has [ text "Turn the light on or off" ]
                        , Query.find [ tag "input", id "radio-inputs-0-option" ]
                            >> Query.has
                                [ attribute (name "radio-inputs")
                                , attribute (required True)
                                ]
                        , Query.find
                            [ tag "label"
                            , attribute (Attrs.for "radio-inputs-0-option")
                            ]
                            >> Query.has [ text "On" ]
                        , Query.find [ tag "input", id "radio-inputs-1-option" ]
                            >> Query.has
                                [ attribute (name "radio-inputs")
                                , attribute (required True)
                                ]
                        , Query.find
                            [ tag "label"
                            , attribute (Attrs.for "radio-inputs-1-option")
                            ]
                            >> Query.has [ text "Off" ]
                        ]
        , test "has errors if no options are provided" <|
            \_ ->
                Field.radio []
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Query.find [ class "errors" ]
                    |> Query.has [ containing [ text "No options have been provided" ] ]
        , test "has invalid options and errors after failed validation" <|
            \_ ->
                let
                    { field } =
                        Interaction.init Parse.bool radioInput
                            |> interact
                                (Query.find [ id "radio-inputs-1-option" ])
                                Event.blur
                in
                field
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ tag "input", id "radio-inputs-0-option" ]
                            >> Query.has
                                [ attribute (name "radio-inputs")
                                , attribute (required True)
                                , attribute (Attrs.attribute "aria-invalid" "true")
                                ]
                        , Query.find [ tag "input", id "radio-inputs-1-option" ]
                            >> Query.has
                                [ attribute (name "radio-inputs")
                                , attribute (required True)
                                , attribute (Attrs.attribute "aria-invalid" "true")
                                ]
                        ]
        ]


dateFieldTests : Test
dateFieldTests =
    describe "date field integration" <|
        [ test "fills date field and parses to correct posix value" <|
            \_ ->
                let
                    dateField =
                        Field.date
                            [ Field.label "Date Field"
                            , Field.name "date-field"
                            , Field.required True
                            ]

                    { result } =
                        Interaction.init Parse.posix dateField
                            |> fillInput "date-field" "2023-12-25"
                in
                case result of
                    Ok posixTime ->
                        Expect.equal (Time.posixToMillis posixTime) 1703462400000

                    Err _ ->
                        Expect.fail "Expected successful parsing of date"
        , test "sets posix value and displays correct string representation" <|
            \_ ->
                let
                    testPosix =
                        Time.millisToPosix 1704067200000

                    dateField =
                        Field.date
                            [ Field.label "Date Field"
                            , Field.name "date-field"
                            , Field.value (Value.time testPosix)
                            , Field.required True
                            ]
                in
                dateField
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Query.find [ tag "input" ]
                    |> Query.has [ attribute (Attrs.attribute "value" "2024-01-01T00:00:00.000") ]
        ]


datetimeFieldTests : Test
datetimeFieldTests =
    describe "datetime field integration" <|
        [ test "fills datetime field and parses to correct posix value" <|
            \_ ->
                let
                    datetimeField =
                        Field.datetime
                            [ Field.label "Datetime Field"
                            , Field.name "datetime-field"
                            , Field.required True
                            ]

                    { result } =
                        Interaction.init Parse.posix datetimeField
                            |> fillInput "datetime-field" "2023-12-25T14:30"
                in
                case result of
                    Ok posixTime ->
                        Expect.equal (Time.posixToMillis posixTime) 1703514600000

                    Err _ ->
                        Expect.fail "Expected successful parsing of datetime"
        , test "sets posix value and displays correct string representation" <|
            \_ ->
                let
                    -- January 1, 2024 at 12:00 UTC
                    testPosix =
                        Time.millisToPosix 1704110400000

                    datetimeField =
                        Field.datetime
                            [ Field.label "Datetime Field"
                            , Field.name "datetime-field"
                            , Field.value (Value.time testPosix)
                            , Field.required True
                            ]
                in
                datetimeField
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Query.find [ tag "input" ]
                    |> Query.has [ attribute (Attrs.attribute "value" "2024-01-01T12:00:00.000") ]
        ]


repeatableFieldsTests : Test
repeatableFieldsTests =
    describe "repeatable fields" <|
        [ test "repeatable inputs" <|
            \_ ->
                let
                    { result } =
                        Interaction.init bandParser bandFields
                            |> fillInput "band-name" "Love and Rockets"
                            |> fillInput "member-name" "Daniel Ash"
                            |> fillInput "member-age" "67"
                            |> clickButton "Add"
                            |> fillInputWithIndex 1 "member-name" "David J"
                            |> fillInputWithIndex 1 "member-age" "67"
                            |> clickButton "Add"
                            |> fillInputWithIndex 2 "member-name" "Kevin Haskins"
                            |> fillInputWithIndex 2 "member-age" "64"
                in
                Expect.equal result
                    (Ok
                        { name = "Love and Rockets"
                        , members =
                            [ { name = "Daniel Ash", age = 67 }
                            , { name = "David J", age = 67 }
                            , { name = "Kevin Haskins", age = 64 }
                            ]
                        }
                    )
        , test "when checkbox is checked, repeatable fields are shown and produce errors when empty" <|
            \_ ->
                let
                    eventField =
                        Field.group []
                            [ Field.text
                                [ Field.label "Event Name"
                                , Field.identifier "event-name"
                                , Field.name "event-name"
                                , Field.required True
                                ]
                            , Field.checkbox
                                [ Field.label "Notify Participants"
                                , Field.identifier "notify-participants"
                                , Field.name "notify-participants"
                                , Field.value (Value.bool True)
                                ]
                            , Field.repeatable
                                [ Field.label "Participants"
                                , Field.repeatableMin 1
                                , Field.identifier "participants"
                                ]
                                (Field.text
                                    [ Field.required True
                                    , Field.identifier "participant-name"
                                    , Field.name "participant-name"
                                    ]
                                )
                                []
                            ]

                    eventParser =
                        Parse.map2
                            (\participants name ->
                                { name = name
                                , participants = participants
                                }
                            )
                            (Parse.field "notify-participants" Parse.bool
                                |> Parse.andUpdate
                                    (\field notify ->
                                        ( Field.updateWithId "participants" (Field.hidden (not notify)) field
                                        , if notify then
                                            Parse.field "participants" (Parse.list Parse.string)

                                          else
                                            Parse.succeed []
                                        )
                                    )
                            )
                            (Parse.field "event-name" Parse.string)

                    interaction =
                        Interaction.init eventParser eventField
                            |> fillInput "event-name" "Team Meeting"
                            |> clickButton "Add Participants"
                            |> check "notify-participants" True
                            |> blur "participant-name"
                in
                interaction
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be provided" ] ]
                        , .result
                            >> Expect.err
                        ]
        , test "when checkbox is unchecked, repeatable fields are hidden and no errors are produced" <|
            \_ ->
                let
                    eventField =
                        Field.group []
                            [ Field.text
                                [ Field.label "Event Name"
                                , Field.identifier "event-name"
                                , Field.name "event-name"
                                , Field.required True
                                ]
                            , Field.checkbox
                                [ Field.label "Notify Participants"
                                , Field.identifier "notify-participants"
                                , Field.name "notify-participants"
                                , Field.value (Value.bool True)
                                ]
                            , Field.repeatable
                                [ Field.label "Participants"
                                , Field.repeatableMin 1
                                , Field.identifier "participants"
                                ]
                                (Field.text
                                    [ Field.required True
                                    , Field.identifier "participant-name"
                                    , Field.name "participant-name"
                                    ]
                                )
                                []
                            ]

                    eventParser =
                        Parse.map2
                            (\participants name ->
                                { name = name
                                , participants = participants
                                }
                            )
                            (Parse.field "notify-participants" Parse.bool
                                |> Parse.andUpdate
                                    (\field notify ->
                                        ( Field.updateWithId "participants" (Field.hidden (not notify)) field
                                        , if notify then
                                            Parse.field "participants" (Parse.list Parse.string)

                                          else
                                            Parse.succeed []
                                        )
                                    )
                            )
                            (Parse.field "event-name" Parse.string)

                    interaction =
                        Interaction.init eventParser eventField
                            |> fillInput "event-name" "Team Meeting"
                            |> clickButton "Add Participants"
                            |> check "notify-participants" False
                            |> blur "participant-name"
                in
                interaction
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.hasNot [ class "errors" ]
                        , .result
                            >> Expect.equal
                                (Ok
                                    { name = "Team Meeting"
                                    , participants = []
                                    }
                                )
                        ]
        ]


validationFocusBlurTests : Test
validationFocusBlurTests =
    describe "validation focus and blur behavior" <|
        [ test "IsBlank error only appears after field is blurred when required" <|
            \_ ->
                Field.int
                    [ Field.required True
                    , Field.name "the-field"
                    ]
                    |> Interaction.init Parse.int
                    |> fillInput "the-field" ""
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.hasNot [ class "errors" ]
                        , blur "the-field"
                            >> .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be provided" ] ]
                        ]
        , test "Range validation errors appear regardless of focus/blur status" <|
            \_ ->
                Field.int
                    [ Field.required True
                    , Field.name "range-field"
                    , Field.min (Value.int 10)
                    , Field.max (Value.int 20)
                    ]
                    |> Interaction.init Parse.int
                    |> fillInput "range-field" "25"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be between 10 and 20" ] ]
                        , blur "range-field"
                            >> .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be between 10 and 20" ] ]
                        ]
        , test "PatternError only appears after field is blurred" <|
            \_ ->
                Field.text
                    [ Field.name "pattern-field"
                    , Field.pattern "({d}{d}{d}) {d}{d}{d}-{d}{d}{d}{d}"
                    ]
                    |> Interaction.init Parse.string
                    |> fillInput "pattern-field" "invalid text"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.hasNot [ class "errors" ]
                        , blur "pattern-field"
                            >> .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Doesn't match the required pattern" ] ]
                        ]
        , test "Valid input gets formatted according to pattern with no errors" <|
            \_ ->
                Field.text
                    [ Field.name "phone-field"
                    , Field.pattern "{d}{d}-{d}{d}"
                    ]
                    |> Interaction.init Parse.string
                    |> fillInput "phone-field" "1234"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ tag "input" ]
                            >> Query.has [ attribute (Attrs.attribute "value" "12-34") ]
                        , .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.hasNot [ class "errors" ]
                        , blur "phone-field"
                            >> .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.hasNot [ class "errors" ]
                        ]
        ]


validationTests : Test
validationTests =
    let
        requiredFieldsForm =
            Field.group []
                [ Field.text
                    [ Field.label "Name"
                    , Field.name "name"
                    , Field.required True
                    ]
                , Field.text
                    [ Field.label "Email"
                    , Field.name "email"
                    , Field.required True
                    ]
                , Field.text
                    [ Field.label "Phone"
                    , Field.name "phone"
                    , Field.required True
                    ]
                ]
    in
    describe "validation tests" <|
        [ test "untouched required fields show no errors in DOM" <|
            \_ ->
                requiredFieldsForm
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Query.hasNot [ class "errors" ]
        , test "after Field.validate errors are displayed for all required fields" <|
            \_ ->
                requiredFieldsForm
                    |> Field.validate
                    |> Field.touch
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Query.findAll [ class "errors" ]
                    |> Query.count (Expect.equal 3)
        ]
