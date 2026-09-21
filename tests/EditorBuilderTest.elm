module EditorBuilderTest exposing (suite)

{- Commands an update returns are dropped: only the model is asserted on.

   Events are simulated through the view, so the sequences follow the browser's.
-}

import Expect exposing (Expectation)
import Internal.Editor.Builder as Builder
import Internal.Editor.Element as Element exposing (Element)
import Json.Encode as Encode
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector, class, id, tag)


suite : Test
suite =
    describe "Editor.Builder"
        [ describe "params form"
            [ test "opens when a node is selected" <|
                \_ ->
                    model
                        |> send Event.click [ class "group-content" ]
                        |> Result.map .paramsOpen
                        |> Expect.equal (Ok True)
            , test "closes when deselected" <|
                \_ ->
                    model
                        |> send Event.click [ class "group-content" ]
                        |> Result.andThen (sendRoot Event.click)
                        |> Result.map .paramsOpen
                        |> Expect.equal (Ok False)
            , test "stays shut when a field is dropped into the tree" <|
                \_ ->
                    model
                        |> send (Event.custom "dragstart" Encode.null) [ class "field-addition" ]
                        |> andThenSend (Event.custom "dragover" dropBefore) [ class "field-element" ]
                        |> andThenSend (Event.custom "drop" Encode.null) [ class "placeholder-element" ]
                        |> Result.map (\dropped -> ( dropped.paramsOpen, groupChildren dropped |> List.length ))
                        |> Expect.equal (Ok ( False, 2 ))
            ]
        , describe "warning icon"
            [ test "is not shown before the form is edited" <|
                \_ ->
                    -- Builder.init's text field has no label
                    model
                        |> Builder.view
                        |> Query.fromHtml
                        |> Query.findAll [ class "warning-icon" ]
                        |> Query.count (Expect.equal 0)
            , test "is not shown for focus and blur alone" <|
                \_ ->
                    model
                        |> send Event.click [ class "field-content" ]
                        |> andThenSend Event.focus [ id "editor-label-1" ]
                        |> andThenSend Event.blur [ id "editor-label-1" ]
                        |> expectRendered
                            (\blurred ->
                                blurred
                                    |> Builder.view
                                    |> Query.fromHtml
                                    |> Query.findAll [ class "warning-icon" ]
                                    |> Query.count (Expect.equal 0)
                            )
            , test "is shown once an invalid node is edited" <|
                \_ ->
                    model
                        |> send Event.click [ class "field-content" ]
                        |> andThenInput [ id "editor-label-1" ] "Given name"
                        |> andThenInput [ id "editor-label-1" ] ""
                        |> expectRendered
                            (\cleared ->
                                cleared
                                    |> Builder.view
                                    |> Query.fromHtml
                                    |> Query.find [ class "warning-icon" ]
                                    |> Query.has [ tag "i" ]
                            )
            ]
        , test "a collapsed group stays collapsed through a form update" <|
            \_ ->
                model
                    |> send Event.click [ class "group-content" ]
                    |> andThenSend Event.click [ class "button-clear" ]
                    |> andThenSend Event.focus [ id "editor-label-1" ]
                    |> expectRendered
                        (\collapsed ->
                            collapsed
                                |> Builder.view
                                |> Query.fromHtml
                                |> Query.find [ class "gg-chevron-right" ]
                                |> Query.has [ tag "i" ]
                        )
        ]


send : ( String, Encode.Value ) -> List Selector -> Builder.Model -> Result String Builder.Model
send event selector model_ =
    model_
        |> Builder.view
        |> Query.fromHtml
        |> Query.findAll selector
        |> Query.first
        |> Event.simulate event
        |> Event.toResult
        |> Result.map (\msg -> Builder.update msg model_ |> Tuple.first)


andThenSend : ( String, Encode.Value ) -> List Selector -> Result String Builder.Model -> Result String Builder.Model
andThenSend event selector =
    Result.andThen (send event selector)


andThenInput : List Selector -> String -> Result String Builder.Model -> Result String Builder.Model
andThenInput selector value =
    Result.andThen
        (\model_ ->
            model_
                |> Builder.view
                |> Query.fromHtml
                |> Query.find selector
                |> Event.simulate (Event.input value)
                |> Event.toResult
                |> Result.map (\msg -> Builder.update msg model_ |> Tuple.first)
        )


{-| The builder root, which is where `Deselect` listens.
-}
sendRoot : ( String, Encode.Value ) -> Builder.Model -> Result String Builder.Model
sendRoot event model_ =
    model_
        |> Builder.view
        |> Query.fromHtml
        |> Event.simulate event
        |> Event.toResult
        |> Result.map (\msg -> Builder.update msg model_ |> Tuple.first)


{-| An event that matches no handler is a failure, not a value to carry along.
-}
expectRendered : (Builder.Model -> Expectation) -> Result String Builder.Model -> Expectation
expectRendered assert result =
    case result of
        Ok rendered ->
            assert rendered

        Err error ->
            Expect.fail ("event produced no message: " ++ error)


{-| Enough of a `dragover` for `Editor.Drag.positionDecoder`.
-}
dropBefore : Encode.Value
dropBefore =
    Encode.object
        [ ( "currentTarget"
          , Encode.object
                [ ( "clientWidth", Encode.int 100 )
                , ( "clientHeight", Encode.int 40 )
                , ( "parentNode", Encode.object [ ( "clientWidth", Encode.int 300 ), ( "clientHeight", Encode.int 200 ) ] )
                ]
          )
        , ( "offsetX", Encode.int 10 )
        , ( "offsetY", Encode.int 40 )
        ]


rootGroup : Builder.Model -> Element
rootGroup model_ =
    Element.elements model_.element
        |> List.head
        |> Maybe.withDefault (Element.root [])


groupChildren : Builder.Model -> List Element
groupChildren model_ =
    rootGroup model_ |> Element.elements


{-| `Builder.init`: a "fields" group holding one text field.
-}
model : Builder.Model
model =
    Builder.init
