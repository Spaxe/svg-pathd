module Svg.PathD exposing
    ( pathD, Segment(..)
    , segment
    , Point
    )

{-| PathD - Minimal SVG Path constructor of the d attribute.

This library helps you specify SVG paths with a clean Elm interface. For the
complete instruction on what each segment does, consult the MDN docs on SVG:
<https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths>

Example of drawing a custom shape:

    import Svg.PathD exposing (Segment(..), d_)

    ...

    Svg.path
        [ d_
            [ M (-0.5, 0.5)
            , Q (0.5, 0.5) (0.5, -0.5)
            , L (-0.5, -0.5)
            , Z
            ]
        ]
        []


# Specifying a SVG Path

@docs pathD, Segment


# Helper methods

@docs segment


# Data Type

@docs Point

-}

import String exposing (fromFloat)
import Svg



-- PATH BUILDER --


{-| Type shorthand for 2 floats that make up a coordinate.
-}
type alias Point =
    ( Float, Float )


{-| Complete implementation of the SVG path `d` attribute.
-}
type Segment
    = M Point
    | L Point
    | H Float
    | V Float
    | Z
    | C Point Point Point
    | S Point Point
    | Q Point Point
    | T Point
    | A Point Float Bool Bool Point


{-| Converts a `Segment` to a partial SVG Path string
-}
segment : Segment -> String
segment s =
    let
        point x y =
            fromFloat x ++ " " ++ fromFloat y

        flag b =
            if b then
                1

            else
                0
    in
    case s of
        M ( x, y ) ->
            "M" ++ point x y

        L ( x, y ) ->
            "L" ++ point x y

        H x ->
            "H" ++ fromFloat x

        V y ->
            "V" ++ fromFloat y

        Z ->
            "Z"

        C ( x1, y1 ) ( x2, y2 ) ( x, y ) ->
            "C" ++ point x1 y1 ++ ", " ++ point x2 y2 ++ ", " ++ point x y

        S ( x2, y2 ) ( x, y ) ->
            "S" ++ point x2 y2 ++ ", " ++ point x y

        Q ( x1, y1 ) ( x, y ) ->
            "Q" ++ point x1 y1 ++ ", " ++ point x y

        T ( x, y ) ->
            "T" ++ point x y

        A ( rx, ry ) angle largeArc sweep ( x, y ) ->
            "A" ++ point rx ry ++ " " ++ fromFloat angle ++ " " ++ fromFloat (flag largeArc) ++ " " ++ fromFloat (flag sweep) ++ " " ++ point x y


{-| Replaces `Svg.Attributes.d`. This function takes a list of Segments and
produces a SVG d attribute with exact specifications.
-}
pathD : List Segment -> String
pathD segs =
    String.join " " <| List.map segment segs
