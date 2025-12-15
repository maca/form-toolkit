module ParseTest exposing (suite)

import Expect
import FormToolkit.Error as Error exposing (Error(..))
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html.Attributes as Attrs
import Json.Decode
import Json.Encode
import Support.ExampleInputs exposing (..)
import Support.Interaction as Interaction
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute)
import Time
import Url


suite : Test
suite =
    describe "Decoding"
        [ describe "succeeds"
            [ test "decoding string" <|
                \_ ->
                    Parse.parse Parse.string stringInput
                        |> Expect.equal (Ok "A string")
            , test "decoding int" <|
                \_ ->
                    Parse.parse Parse.int intInput
                        |> Expect.equal (Ok 1)
            , test "decoding float" <|
                \_ ->
                    Parse.parse Parse.float floatInput
                        |> Expect.equal (Ok 1.1)
            , test "decoding bool" <|
                \_ ->
                    Parse.parse Parse.bool boolInput
                        |> Expect.equal (Ok True)
            , test "decoding posix" <|
                \_ ->
                    Parse.parse Parse.posix posixInput
                        |> Expect.equal (Ok (Time.millisToPosix 0))
            , test "decoding datetime" <|
                \_ ->
                    Parse.parse Parse.posix datetimeInput
                        |> Expect.equal (Ok (Time.millisToPosix 1609459200000))
            , test "decoding custom value with field with options" <|
                \_ ->
                    Field.select
                        [ Field.value (Value.string "Español")
                        , Field.stringOptions [ "Español", "English", "Deutsch" ]
                        ]
                        |> Parse.parse Parse.string
                        |> Expect.equal (Ok "Español")
            , test "decoding field by id" <|
                \_ ->
                    Field.group [] [ stringInput ]
                        |> Parse.parse
                            (Parse.field StringField Parse.string)
                        |> Expect.equal (Ok "A string")
            ]
        , describe "failure"
            [ describe "on simple decoder"
                [ test "produces error" <|
                    \_ ->
                        Parse.parse Parse.int stringInput
                            |> Expect.equal
                                (Err (NotNumber (Just StringField)))
                ]
            , describe "on field decoding"
                [ test "produces error" <|
                    \_ ->
                        Field.group [] [ stringInput ]
                            |> Parse.parse (Parse.field StringField Parse.int)
                            |> Expect.equal (Err (NotNumber (Just StringField)))
                ]
            ]
        , describe "encode json"
            [ test "string" <|
                \_ ->
                    Parse.parse Parse.json stringInput
                        |> Result.withDefault (Json.Encode.string "")
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "string-field" Json.Decode.string)
                        |> Expect.equal (Ok "A string")
            , test "datetime" <|
                \_ ->
                    Parse.parse Parse.json datetimeInput
                        |> Result.withDefault (Json.Encode.string "")
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "datetime-field" Json.Decode.string)
                        |> Expect.equal (Ok "2021-01-01T00:00:00.000")
            , test "group with no name" <|
                \_ ->
                    Parse.parse Parse.json
                        (Field.group [] [ stringInput, intInput ])
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue simpleJsonDecoder
                        |> Expect.equal (Ok ( "A string", 1 ))
            , test "group name" <|
                \_ ->
                    Parse.parse Parse.json groupWithName
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue groupWithNameDecoder
                        |> Expect.equal (Ok ( "A string", 1 ))
            , test "repeatable and group with name" <|
                \_ ->
                    Parse.parse Parse.json
                        (Field.repeatable [ Field.name "repeatable" ]
                            groupWithName
                            []
                        )
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "repeatable"
                                (Json.Decode.list groupWithNameDecoder)
                            )
                        |> Expect.equal
                            (Ok [ ( "A string", 1 ) ])
            , test "repeatable and group with noname" <|
                \_ ->
                    Parse.parse Parse.json
                        (Field.repeatable [ Field.name "repeatable" ]
                            (Field.group [] [ stringInput, intInput ])
                            []
                        )
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "repeatable"
                                (Json.Decode.list simpleJsonDecoder)
                            )
                        |> Expect.equal
                            (Ok [ ( "A string", 1 ) ])
            , test "oneOf succeeds with first matching parser" <|
                \_ ->
                    Parse.parse (Parse.oneOf [ Parse.string, Parse.int |> Parse.map String.fromInt ]) stringInput
                        |> Expect.equal (Ok "A string")
            , test "oneOf succeeds with second parser when first fails" <|
                \_ ->
                    Parse.parse (Parse.oneOf [ Parse.int |> Parse.map String.fromInt, Parse.string ]) stringInput
                        |> Expect.equal (Ok "A string")
            , test "oneOf fails when all parsers fail" <|
                \_ ->
                    Parse.parse (Parse.oneOf [ Parse.int, Parse.fail "Custom error message" ]) stringInput
                        |> Expect.equal (Err (ErrorList (Just StringField) [ NotNumber (Just StringField), CustomError (Just StringField) "Custom error message" ]))
            , test "oneOf with empty list fails" <|
                \_ ->
                    Parse.parse (Parse.oneOf []) stringInput
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        , describe "validates"
            [ test "presence" <|
                \_ ->
                    Parse.parse (Parse.succeed ()) blankInput
                        |> Expect.equal
                            (Err (IsBlank (Just BlankField)))
            , test "errors are presented in correct order" <|
                \_ ->
                    let
                        result =
                            Field.group [] [ stringInput, intInput ]
                                |> Parse.parse
                                    (Parse.succeed (\_ _ -> ())
                                        |> Parse.andMap
                                            (Parse.field StringField Parse.float)
                                        |> Parse.andMap
                                            (Parse.field IntField Parse.float)
                                    )
                    in
                    result
                        |> Expect.equal
                            (Err
                                (ErrorList Nothing
                                    [ NotNumber (Just StringField)
                                    , NotNumber (Just IntField)
                                    ]
                                )
                            )
            , test "errors are not repeated" <|
                \_ ->
                    Field.group [] [ stringInput ]
                        |> Parse.parse
                            (Parse.succeed (\_ _ -> ())
                                |> Parse.andMap
                                    (Parse.field StringField Parse.float)
                                |> Parse.andMap
                                    (Parse.field StringField Parse.float)
                            )
                        |> Expect.equal
                            (Err (NotNumber (Just StringField)))
            , test "email validation only applies to email fields" <|
                \_ ->
                    let
                        invalidEmail =
                            "not-an-email"

                        textField =
                            Field.text
                                [ Field.identifier StringField
                                , Field.value (Value.string invalidEmail)
                                ]

                        emailField =
                            Field.email
                                [ Field.identifier StringField
                                , Field.value (Value.string invalidEmail)
                                ]
                    in
                    Expect.all
                        [ \_ ->
                            Parse.parse Parse.string textField
                                |> Expect.equal (Ok invalidEmail)
                        , \_ ->
                            Parse.parse Parse.string emailField
                                |> Expect.equal (Err (EmailInvalid (Just StringField)))
                        ]
                        ()
            , test "valid email passes validation" <|
                \_ ->
                    let
                        validEmail =
                            "test@example.com"

                        emailField =
                            Field.email
                                [ Field.identifier StringField
                                , Field.value (Value.string validEmail)
                                ]
                    in
                    Parse.parse Parse.string emailField
                        |> Expect.equal (Ok validEmail)
            , test "email validation edge cases" <|
                \_ ->
                    let
                        testEmail email shouldPass =
                            let
                                emailField =
                                    Field.email
                                        [ Field.identifier StringField
                                        , Field.value (Value.string email)
                                        ]

                                result =
                                    Parse.parse Parse.string emailField
                            in
                            if shouldPass then
                                result |> Expect.equal (Ok email)

                            else
                                case result of
                                    Err (EmailInvalid (Just StringField)) ->
                                        Expect.pass

                                    _ ->
                                        Expect.fail ("Expected EmailInvalid for invalid email: " ++ email)
                    in
                    Expect.all
                        [ \_ -> testEmail "valid@example.com" True
                        , \_ -> testEmail "user.name@domain.com" True
                        , \_ -> testEmail "user+tag@example.co.uk" True
                        , \_ -> testEmail "invalid-email" False
                        , \_ -> testEmail "@domain.com" False
                        , \_ -> testEmail "user@" False
                        , \_ -> testEmail "user@example" False
                        , \_ -> testEmail "user@@domain.com" False
                        ]
                        ()
            , test "empty email field validation" <|
                \_ ->
                    let
                        emptyEmailField =
                            Field.email
                                [ Field.identifier StringField
                                , Field.required True
                                ]
                    in
                    -- Empty required email field should fail with IsBlank
                    Parse.parse Parse.string emptyEmailField
                        |> Expect.equal (Err (IsBlank (Just StringField)))
            , test "EmailInvalid error has correct English translation" <|
                \_ ->
                    EmailInvalid (Just StringField)
                        |> Error.toEnglish
                        |> Expect.equal "Please enter a valid email address"
            , test "valid url passes validation" <|
                \_ ->
                    let
                        validUrlString =
                            "https://example.com"

                        urlField =
                            Field.text
                                [ Field.identifier StringField
                                , Field.value (Value.string validUrlString)
                                ]
                    in
                    Parse.parse Parse.url urlField
                        |> Result.map Url.toString
                        |> Expect.equal (Ok "https://example.com/")
            , test "invalid url fails validation" <|
                \_ ->
                    let
                        invalidUrl =
                            "not-a-url"

                        urlField =
                            Field.text
                                [ Field.identifier StringField
                                , Field.value (Value.string invalidUrl)
                                ]
                    in
                    Parse.parse Parse.url urlField
                        |> Expect.equal (Err (UrlInvalid (Just StringField)))
            , test "UrlInvalid error has correct English translation" <|
                \_ ->
                    UrlInvalid (Just StringField)
                        |> Error.toEnglish
                        |> Expect.equal "Please enter a valid URL"
            , test "url validation only applies to url fields" <|
                \_ ->
                    let
                        invalidUrl =
                            "not-a-url"

                        textField =
                            Field.text
                                [ Field.identifier StringField
                                , Field.value (Value.string invalidUrl)
                                ]

                        urlField =
                            Field.url
                                [ Field.identifier StringField
                                , Field.value (Value.string invalidUrl)
                                ]
                    in
                    Expect.all
                        [ \_ ->
                            Parse.parse Parse.string textField
                                |> Expect.equal (Ok invalidUrl)
                        , \_ ->
                            Parse.parse Parse.string urlField
                                |> Expect.equal (Err (UrlInvalid (Just StringField)))
                        ]
                        ()
            , test "valid url with Field.url passes validation" <|
                \_ ->
                    let
                        validUrl =
                            "https://example.com"

                        urlField =
                            Field.url
                                [ Field.identifier StringField
                                , Field.value (Value.string validUrl)
                                ]
                    in
                    Parse.parse Parse.string urlField
                        |> Expect.equal (Ok validUrl)
            , test "url validation edge cases" <|
                \_ ->
                    let
                        testUrl urlString shouldPass =
                            let
                                urlField =
                                    Field.url
                                        [ Field.identifier StringField
                                        , Field.value (Value.string urlString)
                                        ]
                            in
                            if shouldPass then
                                Parse.parse Parse.string urlField
                                    |> Expect.equal (Ok urlString)

                            else
                                Parse.parse Parse.string urlField
                                    |> Expect.equal (Err (UrlInvalid (Just StringField)))
                    in
                    Expect.all
                        [ \_ -> testUrl "https://example.com" True
                        , \_ -> testUrl "http://example.com" True
                        , \_ -> testUrl "https://example.com/path" True
                        , \_ -> testUrl "https://example.com/path?query=value" True
                        , \_ -> testUrl "ftp://example.com" True
                        , \_ -> testUrl "ssh://example.com" True
                        , \_ -> testUrl "custom://example.com" True
                        , \_ -> testUrl "not-a-url" False
                        , \_ -> testUrl "example.com" False
                        , \_ -> testUrl "//example.com" False
                        , \_ -> testUrl "http://" False
                        ]
                        ()
            , test "empty url field validation" <|
                \_ ->
                    let
                        emptyUrlField =
                            Field.url
                                [ Field.identifier StringField
                                , Field.required True
                                ]
                    in
                    Parse.parse Parse.string emptyUrlField
                        |> Expect.equal (Err (IsBlank (Just StringField)))
            , test "parsing json fails when required fields are not filled" <|
                \_ ->
                    let
                        formWithRequiredFields =
                            Field.group []
                                [ Field.text
                                    [ Field.identifier StringField
                                    , Field.name "name"
                                    , Field.required True
                                    ]
                                , Field.text
                                    [ Field.identifier IntField
                                    , Field.name "email"
                                    , Field.required True
                                    ]
                                ]
                    in
                    Parse.parse Parse.json formWithRequiredFields
                        |> Expect.equal
                            (Err
                                (ErrorList Nothing
                                    [ IsBlank (Just StringField)
                                    , IsBlank (Just IntField)
                                    ]
                                )
                            )

            -- , describe "and errors are presented in the correct order" []
            -- , test errors are not repeated after multiple updates
            -- , test group decoding yields error (implemented)
            -- , describe "blank string" should be blank
            ]
        , let
            original =
                "the quick fox jumps over the lazy dog"

            expected =
                "th qck fx jmps vr th lzy dg"
          in
          describe "update input"
            [ test "updates a string affecting parse results" <|
                \_ ->
                    let
                        interaction =
                            stringInput
                                |> Interaction.init
                                    (Parse.string
                                        |> Parse.andUpdate
                                            (\field str ->
                                                ( Field.updateAttribute (Field.stringValue (removeVowels str)) field
                                                , Parse.succeed (removeVowels str)
                                                )
                                            )
                                    )
                                |> Interaction.fillInput "string-field" original
                    in
                    interaction
                        |> Expect.all
                            [ .field
                                >> Field.toHtml (always never)
                                >> Query.fromHtml
                                >> Query.has [ attribute (Attrs.attribute "value" expected) ]
                            , .result >> Expect.equal (Ok expected)
                            ]
            ]
        ]


simpleJsonDecoder : Json.Decode.Decoder ( String, Int )
simpleJsonDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "string-field" Json.Decode.string)
        (Json.Decode.field "int-field" Json.Decode.int)


groupWithNameDecoder : Json.Decode.Decoder ( String, Int )
groupWithNameDecoder =
    Json.Decode.field "group" simpleJsonDecoder


removeVowels : String -> String
removeVowels =
    String.replace "a" ""
        >> String.replace "e" ""
        >> String.replace "i" ""
        >> String.replace "o" ""
        >> String.replace "u" ""
