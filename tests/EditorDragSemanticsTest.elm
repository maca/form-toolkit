module EditorDragSemanticsTest exposing (suite)

{- Reproduces a form-editor bug found by live browser probing
(examples/form-editor via elm-watch):

  Dragging a *Repeatable* from the palette onto a non-empty top level
  silently does nothing, while dragging a *Group* works.

Root cause: Main.draggedOver only splices in a sibling placeholder when the
dragged element is a group

    if not (Element.isGroup element) && isTopLevel then
        -- no placeholder inserted -> drop is a silent no-op
        ...

and Editor.Element.isGroup returns True only for ElementGroup, never for
RepeatableGroup, even though a RepeatableGroup is a container element that
the palette offers for the same top-level use as ElementGroup.

Verified in the running app:
  - palette Group  -> dragover inserts a placeholder, drop adds the group
  - palette Repeatable -> dragover inserts NO placeholder, drop does nothing

The first test below fails against the current code (isGroup returns False);
it is the regression test for the fix. The surrounding tests document the
intended group semantics.
-}

import Editor.Element as Element
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Editor.Element.isGroup (drag & drop semantics)"
        [ test "an ElementGroup is a group" <|
            \_ ->
                Element.isGroup Element.group
                    |> Expect.equal True
        , test "a RepeatableGroup is a group (palette Repeatable must drop beside top-level groups)" <|
            \_ ->
                Element.isGroup Element.repeatableGroup
                    |> Expect.equal True
        , test "a RepeatableGroup nested in another group is a group" <|
            \_ ->
                Element.group
                    |> Element.prepend Element.repeatableGroup
                    |> Element.elements
                    |> List.all Element.isGroup
                    |> Expect.equal True
        , test "a plain field is not a group" <|
            \_ ->
                Element.isGroup Element.text
                    |> Expect.equal False
        ]
