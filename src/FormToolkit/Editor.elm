module FormToolkit.Editor exposing
    ( Model, Msg, init, update
    , view, viewCompact, preview
    , save, load
    )

{-| The drag-and-drop form builder: a palette of fields to drag in, the tree of
elements, and a form for editing the properties of the selected element.

The builder is a self-contained component. Keep its [Model](#Model) in your own
model, feed it the [Msg](#Msg)s produced by its views, and run the `Cmd`s
[update](#update) returns. Render it with [view](#view) or
[viewCompact](#viewCompact), and preview the form it describes with
[preview](#preview).

@docs Model, Msg, init, update


# Views

@docs view, viewCompact, preview


# Serialization

@docs save, load

-}

import Html exposing (Html)
import Internal.Editor.Builder as Builder
import Internal.Editor.Element as Element
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| Represents the state of a Form editor
-}
type Model
    = Model Builder.Model


{-| A message to update the Form editor, any user interaction is encoded in
this type, be it dragging and reordering form elements, or changing label or any
other parameter
-}
type Msg
    = Msg Builder.Msg


{-| Initialize a Form editor
-}
init : Model
init =
    Model Builder.init


{-| Update an Editor model with a message, you would probably call from the
`update` function of a parent Elm app.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update (Msg builderMsg) (Model model) =
    let
        ( updated, cmd ) =
            Builder.update builderMsg model
    in
    ( Model updated, Cmd.map Msg cmd )


{-| Render the editor UI in a three column layout.
-}
view : Model -> Html Msg
view (Model model) =
    Html.map Msg (Builder.view model)


{-| Render a compact presentation of the editor UI in a two column layout with a
sliding form element parameters form.
-}
viewCompact : Model -> Html Msg
viewCompact (Model model) =
    Html.map Msg (Builder.viewCompact model)


{-| Render the preview UI of form being built.
-}
preview : Model -> Html ()
preview (Model model) =
    Builder.preview model


{-| Serialize the form definition to JSON, or `Nothing` for an empty tree.

Load it back with [load](#load). The builder's transient state — selection,
dragging, collapsed groups — is not saved.

        import FormToolkit.Editor as Editor
        import Json.Encode as Encode

        Editor.init
            |> Editor.save
                |> Maybe.map (Encode.encode 0)
            --> Just "{\"type\":\"group\",\"fields\":[{\"type\":\"group\",\"fields\":[{\"type\":\"text\",\"name\":\"text_field\"}],\"name\":\"fields\"}],\"name\":\"root\"}"

-}
save : Model -> Maybe Encode.Value
save (Model model) =
    Element.encode model.element


{-| Load a form definition from the JSON produced by [save](#save), ready to be
rendered.
-}
load : Decoder Model
load =
    Decode.map (\element -> Model (Builder.withElement element Builder.init)) Element.decode
