module CalculatorTests exposing (..)

import Calculator exposing (dissectShapes, orderToSolve)
import Expect exposing (..)
import Shapes exposing (Shape2D(..), Shape3D(..))
import Statues.Internal exposing (Position(..))
import Test exposing (..)
import Shapes exposing (isCone, isCylinder, isPrism, isCube)


sphere : Shape3D
sphere = Extrusion Circle Circle

cube : Shape3D
cube = Extrusion Square Square

pyramid : Shape3D
pyramid = Extrusion Triangle Triangle

prism : Shape3D
prism = Extrusion Square Triangle

cylinder : Shape3D
cylinder = Extrusion Circle Square

cone : Shape3D
cone = Extrusion Circle Triangle

mapBothWithDefault : (x, x) -> (Shape3D -> x) -> (Shape3D -> x) -> Maybe (Shape3D, Shape3D) -> (x, x)
mapBothWithDefault default f1 f2 s = Maybe.map (\(s1, s2) -> (f1 s1, f2 s2)) s |> Maybe.withDefault default

dissectShapesTests : Test
dissectShapesTests =
    let
        checkShapes = mapBothWithDefault (False, False)  
    in
    describe "Dissecting two shapes"
        [ describe "Circle"
            [ test "should swap with Triangle" <|
                \_ -> dissectShapes ( Circle, sphere ) ( Triangle, prism ) 
                        |> checkShapes isCone isCylinder
                        |> Expect.equal (True, True)
            , test "should swap with Square" <|
                \_ -> dissectShapes ( Circle, cone ) ( Square, prism )
                        |> checkShapes isPrism isCone
                        |> Expect.equal (True, True)
            , test "should not be able to swap" <|
                \_ -> dissectShapes ( Circle,  prism) ( Triangle, sphere )
                        |> Expect.equal Nothing
            ]
        , describe "Sqaure"
            [ test "should swap with Circle" <|
                \_ -> dissectShapes (Square, cube) (Circle, sphere)
                         |> checkShapes isCylinder isCylinder
                         |> Expect.equal (True, True)
            , test "should swap with Triangle" <|
                \_ -> dissectShapes ( Square, cylinder ) ( Triangle, cone)
                        |> checkShapes isCone isCylinder
                        |> Expect.equal (True, True) 
            , test "should not be able to swap" <|
                \_ -> dissectShapes ( Square, cone ) (Circle, prism)
                        |> Expect.equal Nothing
            ]
        , describe "Triangle"
            [ test "should swap with Circle" <|
                \_ -> dissectShapes ( Triangle, pyramid ) ( Circle, sphere )
                        |> checkShapes isCone isCone
                        |> Expect.equal (True, True)
            , test "should swap with Square" <|
                \_ -> dissectShapes ( Triangle, prism) (Square, cylinder)
                        |> checkShapes isCube isCone
                        |> Expect.equal (True, True)
            , test "should not be able to swap" <|
                \_ -> dissectShapes (Triangle, cylinder) (Circle, prism)
                        |> Expect.equal Nothing
            ]
        ]


orderToSolveTests : Test
orderToSolveTests =
    let
        getOrderedPositions = orderToSolve >> List.map (\s -> s.position)
    in
    describe "Reorder list of statues in order of what to solve first"
        [ test "should re order to M, R, L" <|
            \_ ->
                [ { insideShape = Triangle 
                  , outsideShape = Extrusion Square Triangle
                  , position = Right
                  }
                , { insideShape = Square
                  , outsideShape = Extrusion Square Triangle
                  , position = Middle
                  }
                , { insideShape = Circle
                  , outsideShape = Extrusion Circle Circle
                  , position = Left
                  }
                ]
                    |> getOrderedPositions
                    |> Expect.equal [ Middle, Right, Left ]
        , test "should re order to L, M, R" <|
            \_ ->
                [ { insideShape = Triangle
                  , outsideShape = Extrusion Circle Circle
                  , position = Left
                  }
                , { insideShape = Circle
                  , outsideShape = Extrusion Square Square
                  , position = Middle
                  }
                , { insideShape = Square
                  , outsideShape = Extrusion Triangle Triangle
                  , position = Right
                  }
                ]
                    |> getOrderedPositions
                    |> Expect.equal [ Left, Middle, Right ]
        , test "should re order to L, R, M" <|
            \_ ->
                [ { insideShape = Triangle
                  , outsideShape = Extrusion Circle Triangle
                  , position = Right
                  }
                , { insideShape = Square
                  , outsideShape = Extrusion Square Square
                  , position = Middle
                  }
                , { insideShape = Circle
                  , outsideShape = Extrusion Circle Triangle
                  , position = Left
                  }
                ]
                    |> getOrderedPositions
                    |> Expect.equal [ Left, Right, Middle ]
        , test "should re order to L, M, R when number of steps are the same (2)" <|
            \_ ->
                [ { insideShape = Triangle
                  , outsideShape = Extrusion Triangle Triangle
                  , position = Middle
                  }
                , { insideShape = Square
                  , outsideShape = Extrusion Square Square
                  , position = Right
                  }
                , { insideShape = Circle
                  , outsideShape = Extrusion Circle Circle
                  , position = Left
                  }
                ]
                    |> getOrderedPositions
                    |> Expect.equal [ Left, Middle, Right ]
        , test "should re order to L, M, R when number of steps are the same (1)" <|
            \_ ->
                [ { insideShape = Square
                  , outsideShape = Extrusion Circle Square
                  , position = Middle
                  }
                , { insideShape = Triangle
                  , outsideShape = Extrusion Square Triangle
                  , position = Right
                  }
                , { insideShape = Circle
                  , outsideShape = Extrusion Circle Triangle
                  , position = Left
                  }
                ]
                    |> getOrderedPositions
                    |> Expect.equal [ Left, Middle, Right ]
        ]




