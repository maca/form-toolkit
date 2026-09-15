module Editor.Drag exposing
    ( Drag(..), idle
    , Position(..), positionDecoder
    )

{-| Per-node drag state for the form builder, and where a drop lands relative to
its target.


# Drag state

@docs Drag, idle


# Drop position

@docs Position, positionDecoder

-}

import Json.Decode as Decode exposing (Decoder)


type Position
    = Before
    | After


type Drag
    = Idle
    | Enabled
    | Dragged


type alias Coords =
    { x : Int
    , y : Int
    }


type alias Dimensions =
    { width : Int
    , height : Int
    }


idle : Drag
idle =
    Idle


dimensionsDecoder : Decoder Dimensions
dimensionsDecoder =
    Decode.map2 Dimensions
        (Decode.field "clientWidth" Decode.int)
        (Decode.field "clientHeight" Decode.int)


coordsDecoder : Decoder Coords
coordsDecoder =
    Decode.map2 Coords
        (Decode.field "offsetX" Decode.float |> Decode.map round)
        (Decode.field "offsetY" Decode.float |> Decode.map round)


positionDecoder : Decoder Position
positionDecoder =
    Decode.map3 decodePosition
        (Decode.field "currentTarget" dimensionsDecoder)
        (Decode.at [ "currentTarget", "parentNode" ] dimensionsDecoder)
        coordsDecoder


decodePosition : Dimensions -> Dimensions -> Coords -> Position
decodePosition target parent { x, y } =
    if target.width <= parent.width // 2 then
        if x < target.width // 4 then
            Before

        else
            After

    else if y < target.height // 2 then
        Before

    else
        After
