module Svg.PathD exposing
    ( pathD, Segment(..)
    , a, c, h, l, m, q, s, t, v, z
    )

{-| PathD - Minimal, complete SVG Path constructor of the <svg> data (`d`) attribute.

This library helps you specify SVG paths with a clean Elm interface. For the
complete instruction on what each segment does, consult the MDN docs on SVG:
<https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths>

Example of drawing a custom shape:

    import Svg
    import Svg.PathD exposing (Segment(..), pathD, m, l)

    ...

    Svg.path
        [ d <| pathD
            [ M (-0.5, 0.5)
            , Q (0.5, 0.5) (0.5, -0.5)
            , L (-0.5, -0.5)
            , m (0.1, 0.1)
            , l (0.2, 0.1)
            , l (0.2, 0.25)
            , Z
            ]
        ]
        []

Uppercase commands work in absolute coordinates, and lowercase commands work in relative
coordinates. For how relative paths work, see <https://oreillymedia.github.io/Using_SVG/guide/path-data.html>


# Specifying a SVG Path

@docs pathD, Segment


# Helper methods

@docs a, c, h, l, m, q, s, t, v, z


# Data Type

@docs Point

-}

import String exposing (fromFloat)



-- PATH BUILDER --


{-| Type shorthand for 2 floats that make up a coordinate.
-}
type alias Point =
    ( Float, Float )


{-| Complete implementation of the SVG path `d` attribute. For relative commands
in corresponding lowercase, such as `m` or `l`, they are exposed from the
top-level module.

    Construtors ending with `-d` are implementation details, and will be removed
    in a future iteration. Use the corresponding lowercase function instead.

    Qualifying imports may be used to resolve namespace clashing, like

        import Svg.PathD as PathD exposing (Segment(..), m, l)

    The commands will then be accessible as `PathD.M`, `PathD.Q`, `PathD.m` etc.

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
      -- relative commands
    | Md Point
    | Ld Point
    | Hd Float
    | Vd Float
    | Zd
    | Cd Point Point Point
    | Sd Point Point
    | Qd Point Point
    | Td Point
    | Ad Point Float Bool Bool Point


{-| Converts a `Segment` to a partial SVG Path string
-}
segment : Segment -> String
segment seg =
    let
        point x y =
            fromFloat x ++ " " ++ fromFloat y

        flag b =
            if b then
                1

            else
                0
    in
    case seg of
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

        Md ( x, y ) ->
            "m" ++ point x y

        Ld ( x, y ) ->
            "l" ++ point x y

        Hd x ->
            "h" ++ fromFloat x

        Vd y ->
            "v" ++ fromFloat y

        Zd ->
            "z"

        Cd ( x1, y1 ) ( x2, y2 ) ( x, y ) ->
            "c" ++ point x1 y1 ++ ", " ++ point x2 y2 ++ ", " ++ point x y

        Sd ( x2, y2 ) ( x, y ) ->
            "s" ++ point x2 y2 ++ ", " ++ point x y

        Qd ( x1, y1 ) ( x, y ) ->
            "q" ++ point x1 y1 ++ ", " ++ point x y

        Td ( x, y ) ->
            "t" ++ point x y

        Ad ( rx, ry ) angle largeArc sweep ( x, y ) ->
            "a" ++ point rx ry ++ " " ++ fromFloat angle ++ " " ++ fromFloat (flag largeArc) ++ " " ++ fromFloat (flag sweep) ++ " " ++ point x y


{-| Declare a relative move-to SVG path command
-}
m : Point -> Segment
m =
    Md


{-| Declare a relative line-to SVG path command
-}
l : Point -> Segment
l =
    Ld


{-| Declare a relative horizontal line SVG path command
-}
h : Float -> Segment
h =
    Hd


{-| Declare a relative vertical line SVG path command
-}
v : Float -> Segment
v =
    Vd


{-| Declare a relative close-path SVG path command
-}
z : Segment
z =
    Zd


{-| Declare a relative cubic curve-to SVG path command
-}
c : Point -> Point -> Point -> Segment
c =
    Cd


{-| Declare a relative smooth cubic curve-to SVG path command
-}
s : Point -> Point -> Segment
s =
    Sd


{-| Declare a relative quadratic curve-to SVG path command
-}
q : Point -> Point -> Segment
q =
    Qd


{-| Declare a relative smooth quadratic curve-to SVG path command
-}
t : Point -> Segment
t =
    Td


{-| Declare a relative arc SVG path command
-}
a : Point -> Float -> Bool -> Bool -> Point -> Segment
a =
    Ad


{-| This function takes a list of Segments and produces a SVG d attribute with exact specifications.
-}
pathD : List Segment -> String
pathD segs =
    String.join " " <| List.map segment segs
