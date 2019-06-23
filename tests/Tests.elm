module Tests exposing (suite)

import Expect
import Svg
import Svg.PathD exposing (Segment(..), a, c, h, l, m, pathD, q, s, t, v, z)
import Test exposing (..)


suite : Test
suite =
    describe "The Svg.PathD Module"
        [ test "can create straight path segments" <|
            \_ ->
                Expect.equal "M10 10 L0 100 H5 V-5 Z" <|
                    pathD
                        [ M ( 10, 10 )
                        , L ( 0, 100 )
                        , H 5
                        , V -5
                        , Z
                        ]
        , test "can create bezier segments" <|
            \_ ->
                Expect.equal "C0 0, 5 10, 15 20 S19 25, 21 45 Q1 2, 4 5 T34 45 A6 6 180 1 0 4 4 Z" <|
                    pathD
                        [ C ( 0, 0 ) ( 5, 10 ) ( 15, 20 )
                        , S ( 19, 25 ) ( 21, 45 )
                        , Q ( 1, 2 ) ( 4, 5 )
                        , T ( 34, 45 )
                        , A ( 6, 6 ) 180 True False ( 4, 4 )
                        , Z
                        ]
        , test "can create mixed absolute and relative segments" <|
            \_ ->
                Expect.equal "C0 0, 5 10, 15 20 s19 25, 21 45 Q1 2, 4 5 t34 45 A6 6 180 1 0 4 4 l0 100 H5 v-5 Z" <|
                    pathD
                        [ C ( 0, 0 ) ( 5, 10 ) ( 15, 20 )
                        , s ( 19, 25 ) ( 21, 45 )
                        , Q ( 1, 2 ) ( 4, 5 )
                        , t ( 34, 45 )
                        , A ( 6, 6 ) 180 True False ( 4, 4 )
                        , l ( 0, 100 )
                        , H 5
                        , v -5
                        , Z
                        ]
        ]
