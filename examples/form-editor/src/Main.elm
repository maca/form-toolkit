module Main exposing (main)

import Browser
import FormToolkit.Editor as Editor
import Html exposing (Html, button, div, text)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { editor : Editor.Model
    , activeTab : Tab
    , jsonInput : String
    , jsonError : Maybe String
    }


type Tab
    = PreviewTab
    | JsonTab


type Msg
    = EditorMsg Editor.Msg
    | TabSwitched Tab
    | JsonInputChanged String
    | JsonImportClicked
    | NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( { editor = Editor.init
      , activeTab = PreviewTab
      , jsonInput = ""
      , jsonError = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            let
                ( editor, editorCmd ) =
                    Editor.update editorMsg model.editor
            in
            ( { model | editor = editor }, Cmd.map EditorMsg editorCmd )

        TabSwitched tab ->
            ( { model | activeTab = tab }, Cmd.none )

        JsonInputChanged input ->
            ( { model | jsonInput = input }, Cmd.none )

        JsonImportClicked ->
            ( case Decode.decodeString Editor.load model.jsonInput of
                Ok decoded ->
                    { model | editor = decoded, jsonError = Nothing }

                Err error ->
                    { model | jsonError = Just (Decode.errorToString error) }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "editor-layout" ]
        [ div [ class "schema-editor-container" ]
            [ Html.map EditorMsg (Editor.view model.editor) ]
        , div [ class "editor-preview-pane" ]
            [ div [ class "tab-nav" ]
                [ tabButton "Preview" PreviewTab model.activeTab
                , tabButton "JSON" JsonTab model.activeTab
                ]
            , div [ class "tab-content" ]
                (case model.activeTab of
                    PreviewTab ->
                        [ div [ class "tab-pane" ]
                            [ Html.map (always NoOp) (Editor.preview model.editor) ]
                        ]

                    JsonTab ->
                        [ div [ class "tab-pane" ]
                            [ case Editor.save model.editor of
                                Just json ->
                                    Html.pre [] [ text (Encode.encode 2 json) ]

                                Nothing ->
                                    text "Nothing to serialize"
                            , Html.hr [] []
                            , Html.textarea
                                [ Attributes.value model.jsonInput
                                , Events.onInput JsonInputChanged
                                , Attributes.placeholder "Paste form JSON here to load it"
                                ]
                                []
                            , button [ Events.onClick JsonImportClicked ] [ text "Load" ]
                            , case model.jsonError of
                                Just error ->
                                    div [ class "json-error" ] [ text error ]

                                Nothing ->
                                    text ""
                            ]
                        ]
                )
            ]
        ]


tabButton : String -> Tab -> Tab -> Html Msg
tabButton label activeTab currentTab =
    button
        [ classList
            [ ( "tab-button", True )
            , ( "active", currentTab == activeTab )
            ]
        , Events.onClick (TabSwitched activeTab)
        ]
        [ text label ]
