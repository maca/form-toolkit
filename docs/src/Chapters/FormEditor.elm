module Chapters.FormEditor exposing (Model, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Editor as Editor
import Html exposing (div, pre, text)
import Html.Attributes as Attributes
import Json.Encode as Encode


type alias Book book =
    { book | formEditor : Model }


type alias Model =
    { builder : Editor.Model }


init : Model
init =
    { builder = Editor.init }


{-| Takes the book state so the builder's own commands can be re-dispatched
like view events.
-}
update : Editor.Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update builderMsg book =
    let
        ( builder, builderCmd ) =
            Editor.update builderMsg book.formEditor.builder
    in
    ( { book | formEditor = { builder = builder } }
    , Cmd.map (Actions.updateStateWithCmdWith update) builderCmd
    )


chapter : Chapter (Book book)
chapter =
    Chapter.chapter "Form Editor"
        |> Chapter.withStatefulComponentList
            [ ( "Builder"
              , \book ->
                    book.formEditor.builder
                        |> Editor.viewCompact
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Preview"
              , \book ->
                    div [ Attributes.class "milligram form-editor-preview" ]
                        [ book.formEditor.builder
                            |> Editor.preview
                            |> Html.map (Actions.updateStateWithCmdWith (\_ state -> ( state, Cmd.none )))
                        ]
              )
            , ( "JSON"
              , \book ->
                    div [ Attributes.class "form-editor-json" ]
                        [ case Editor.save book.formEditor.builder of
                            Just json ->
                                pre [] [ text (Encode.encode 2 json) ]

                            Nothing ->
                                text "Nothing to serialize"
                        ]
              )
            ]
        |> Chapter.render markdownContent


markdownContent : String
markdownContent =
    """
Build a form by dragging elements from the palette into the tree. Picking an
element slides its properties in over the palette, and clicking anywhere else
puts them away.

<component with-label="Builder"/>

<component with-label="Preview"/>

<component with-label="JSON"/>
"""
