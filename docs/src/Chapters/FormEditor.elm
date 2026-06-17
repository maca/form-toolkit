module Chapters.FormEditor exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Chapter as Chapter exposing (Chapter)


type alias Model =
    {}


type Msg
    = NoOp


init : Model
init =
    {}


update : Msg -> Model -> ( Model, Cmd (ElmBook.Msg state) )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


chapter : Chapter { x | formEditor : Model }
chapter =
    Chapter.chapter "Form Editor"
        |> Chapter.render markdownContent


markdownContent : String
markdownContent =
    """
# Form Editor

Interactive form editor for building forms dynamically.

*Coming soon...*
"""
