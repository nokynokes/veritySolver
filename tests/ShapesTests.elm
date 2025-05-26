module ShapesTests exposing (..)

import Shapes exposing (Shape2D(..), Shape3D(..), combine, subtract, shapesMissing, isComplete, hasShapes, removeForDoubleShapes, isSphere, isCube, isPyramid, isCylinder, isCone, isPrism)
import Test exposing (..)
import Expect exposing (..)

checkBoth : (a -> x) -> ( a, a ) -> ( x, x )
checkBoth f =
    Tuple.mapBoth f f

combineShapesTests : Test
combineShapesTests =
    describe "combineShapes"
        [ (test "should combine two circles into a sphere" <|
            \_ ->
                combine Circle Circle
                    |> isSphere
                    |> Expect.equal True)
        , (test "should combine two squares into a cube" <|
            \_ ->
                combine Square Square
                    |> isCube
                    |> Expect.equal True)
        , (test "should combine two triangles into a pyramid" <|
            \_ ->
                combine Triangle Triangle
                    |> isPyramid
                    |> Expect.equal True)
        , (test "should combine a circle and a square into a cylinder" <|
            \_ ->
                (combine Circle Square, combine Square Circle)
                    |> checkBoth isCylinder
                    |> Expect.equal (True, True))
        , (test "should combine a circle and a triangle into a cone" <|
            \_ ->
                (combine Circle Triangle, combine Triangle Circle)
                    |> checkBoth isCone
                    |> Expect.equal (True, True))
        , (test "should combine a square and a triangle into a prism" <|
            \_ ->
                (combine Square Triangle, combine Triangle Square)
                    |> checkBoth isPrism
                    |> Expect.equal (True, True))
        ]

subtractShapesTests : Test
subtractShapesTests =
    describe "subtractShapes"
        [ (test "should subtract a circle from a sphere" <|
            \_ ->
                subtract Circle (Extrusion Circle Circle)
                    |> Expect.equal (Just Circle))
        , (test "should subtract a square from a cube" <|
            \_ ->
                subtract Square (Extrusion Square Square)
                    |> Expect.equal (Just Square))
        , (test "should subtract a triangle from a pyramid" <|
            \_ ->
                subtract Triangle (Extrusion Triangle Triangle)
                    |> Expect.equal (Just Triangle))
        , (test "should subtract a circle from a cylinder" <|
            \_ ->
                (subtract Circle (Extrusion Circle Square), subtract Circle (Extrusion Square Circle))
                    |> Expect.equal (Just Square, Just Square))
        , (test "should subtract a square from a cylinder" <|
            \_ ->
                (subtract Square (Extrusion Circle Square), subtract Square (Extrusion Square Circle))
                    |> Expect.equal (Just Circle, Just Circle))
        , (test "should subtract a circle from a cone" <|
            \_ ->
                (subtract Circle (Extrusion Circle Triangle), subtract Circle (Extrusion Triangle Circle))
                    |> Expect.equal (Just Triangle, Just Triangle))
        , (test "should subtract a triangle from a cone" <|
            \_ ->
                (subtract Triangle (Extrusion Circle Triangle), subtract Triangle (Extrusion Triangle Circle))
                    |> Expect.equal (Just Circle, Just Circle))
        , (test "should subtract a square from a prism" <|
            \_ ->
                (subtract Square (Extrusion Square Triangle), subtract Square (Extrusion Triangle Square))
                    |> Expect.equal (Just Triangle, Just Triangle))
        , (test "should subtract a triangle from a prism" <|
            \_ ->
                (subtract Triangle (Extrusion Square Triangle), subtract Triangle (Extrusion Triangle Square))
                    |> Expect.equal (Just Square, Just Square))
        , (test "should not subtract a square or triangle from a sphere" <|
            \_ ->
                (subtract Square (Extrusion Circle Circle), subtract Triangle (Extrusion Circle Circle))
                    |> Expect.equal (Nothing, Nothing))
        , (test "should not subtract a circle or triangle from a cube" <|
            \_ ->
                (subtract Circle (Extrusion Square Square), subtract Triangle (Extrusion Square Square))
                    |> Expect.equal (Nothing, Nothing))
        , (test "should not subtract a circle or square from a pyramid" <|
            \_ ->
                (subtract Circle (Extrusion Triangle Triangle), subtract Square (Extrusion Triangle Triangle))
                    |> Expect.equal (Nothing, Nothing))
        , (test "should not subtract a triangle from a cylinder" <|
            \_ ->
                (subtract Triangle (Extrusion Circle Square), subtract Triangle (Extrusion Square Circle))
                    |> Expect.equal (Nothing, Nothing))
        , (test "should not subtract a square from a cone" <|
            \_ ->
                (subtract Square (Extrusion Circle Triangle), subtract Square (Extrusion Triangle Circle))
                    |> Expect.equal (Nothing, Nothing))
        , (test "should not subtract a circle from a prism" <|
            \_ ->
                (subtract Circle (Extrusion Square Triangle), subtract Circle (Extrusion Triangle Square))
                    |> Expect.equal (Nothing, Nothing))
        ]
        
illegalShapeToStartTests : Test
illegalShapeToStartTests =
    describe "illegalShapeToStart"
        [ (test "should return true for circle and prism" <|
            \_ -> 
                let
                    isCompleteCircle = isComplete Circle  
                in
                    (isCompleteCircle (Extrusion Square Triangle), isCompleteCircle (Extrusion Triangle Square))
                        |> Expect.equal (True, True))
        , (test "should return true for square and cone" <|
            \_ -> 
                let
                    isCompleteSquare = isComplete Square  
                in
                    (isCompleteSquare (Extrusion Circle Triangle), isCompleteSquare (Extrusion Triangle Circle))
                        |> Expect.equal (True, True))
        , (test "should return true for triangle and cylinder" <|
            \_ -> 
                let
                    isCompleteTriangle = isComplete Triangle  
                in
                    (isCompleteTriangle (Extrusion Circle Square), isCompleteTriangle (Extrusion Square Circle))
                        |> Expect.equal (True, True))
        ]

shapesMissingTest : Test
shapesMissingTest = 
    describe "shapesMissingTest"
        [ test "Square and Prism should return Circle" <|
            \_ ->
                shapesMissing Square (Extrusion Square Triangle)
                    |> Expect.equal (Just [ Circle ])
                    
        , test "Square and Cylinder should return Triangle" <|
            \_ ->
                shapesMissing Square (Extrusion Square Circle)
                    |> Expect.equal (Just [ Triangle ])
                    
        , test "Square and Cube should return Triangle and Circle" <|
            \_ ->
                shapesMissing Square (Extrusion Square Square)
                    |> Expect.equal (Just [ Triangle, Circle ])
                    
        , test "Triangle and Prism should return Circle" <|
            \_ ->
                shapesMissing Triangle (Extrusion Square Triangle)
                    |> Expect.equal (Just [ Circle ])
                    
        , test "Triangle and Cone should return Square" <|
            \_ ->
                shapesMissing Triangle (Extrusion Triangle Circle)
                    |> Expect.equal (Just [ Square ])
                    
        , test "Triangle and Cube should return Circle" <|
            \_ ->
                shapesMissing Triangle (Extrusion Square Square)
                    |> Expect.equal (Just [ Circle ])
                    
        , test "Triangle and Cylinder should return Nothing" <|
            \_ ->
                shapesMissing Triangle (Extrusion Circle Square)
                    |> Expect.equal (Nothing)
                    
        , test "Circle and Cylinder should return Triangle" <|
            \_ ->
                shapesMissing Circle (Extrusion Circle Square)
                    |> Expect.equal (Just [ Triangle ])
                    
        , test "Circle and Cone should return Square" <|
            \_ ->
                shapesMissing Circle (Extrusion Triangle Circle)
                    |> Expect.equal (Just [ Square ])
                    
        , test "Circle and Cube should return Triangle" <|
            \_ ->
                shapesMissing Circle (Extrusion Square Square)
                    |> Expect.equal (Just [ Triangle ])
                    
        , test "Circle and Sphere should return Square and Triangle" <|
            \_ ->
                shapesMissing Circle (Extrusion Circle Circle)
                    |> Expect.equal (Just [ Square, Triangle ])
                    
        , test "Circle and Prism should return Nothing" <|
            \_ ->
                shapesMissing Circle (Extrusion Square Triangle)
                    |> Expect.equal Nothing
                    
        , test "Sqaure and Pryamid should return Circle" <|
            \_ ->
                shapesMissing Square (Extrusion Triangle Triangle)
                    |> Expect.equal (Just [ Circle ])
                    
        , test "Circle and Pryamid should return Square" <|
            \_ ->
                shapesMissing Circle (Extrusion Triangle Triangle)
                    |> Expect.equal ( Just [ Square ])
        ]

hasShapesTest : Test 
hasShapesTest = 
    let
        hasShapesCircle = hasShapes [Circle]
        hasShapesSqaure = hasShapes [Square]
        hasShapesTriangle = hasShapes [Triangle]
        hasShapesAll2D = hasShapes [Circle, Square, Triangle]
        sphere = Extrusion Circle Circle
        cube = Extrusion Square Square
        pyramid = Extrusion Triangle Triangle
        cylinder = Extrusion Circle Square
        reverseCylinder = Extrusion Square Circle
        cone = Extrusion Circle Triangle
        reverseCone = Extrusion Triangle Circle
        prism = Extrusion Square Triangle
        reversePrism = Extrusion Triangle Square
        expectTrue = Expect.equal True
        expectFalse = Expect.equal False
    in
        describe "hasShapesTest"
            [  test "should return true for Circle and Sphere" <|
                \_ -> 
                    hasShapesCircle sphere |> expectTrue
            ,  test "should return true for Square and Cube" <|
                \_ -> 
                    hasShapesSqaure cube |> expectTrue
            ,  test "should return true for Triangle and pyramid" <|
                \_ -> 
                    hasShapesTriangle pyramid |> expectTrue
            ,  test "should return true for Circle and Cylinder" <|
                \_ -> 
                    hasShapesCircle cylinder && hasShapesCircle reverseCylinder |> expectTrue
            ,  test "should return true for Circle and Cone" <|
                \_ -> 
                    hasShapesCircle cone && hasShapesCircle reverseCone |> expectTrue
            ,  test "should return true for Square and Cylinder" <|
                \_ -> 
                    hasShapesSqaure cylinder && hasShapesSqaure reverseCylinder |> expectTrue
            ,  test "should return true for Square and Prisim" <|
                \_ -> 
                    hasShapesSqaure prism && hasShapesSqaure reversePrism |> expectTrue
            ,  test "should return true for Triangle and Cone" <|
                \_ -> 
                    hasShapesTriangle cone && hasShapesTriangle reverseCone |> expectTrue
            ,  test "should return true for Triangle and Prism" <|
                \_ -> 
                    hasShapesTriangle prism && hasShapesTriangle reversePrism |> expectTrue
            ,  test "should return true for all 2d shapes regardless of 3d shape" <|
                \_ -> 
                    hasShapesAll2D cube && hasShapesAll2D sphere && hasShapesAll2D pyramid |> expectTrue
            ,  test "should return False for Square and Sphere" <|
                \_ -> 
                    hasShapesSqaure sphere |> expectFalse
            ]

removeForDoubleShapesTest : Test
removeForDoubleShapesTest = 
    describe "removeForDouble" 
        [  test "should return a cricle for a sphere" <|
            \_ -> 
                removeForDoubleShapes (Extrusion Circle Circle) |> Expect.equal (Just Circle)
        ,  test "should return a square for a cube" <|
            \_ -> 
                removeForDoubleShapes (Extrusion Square Square) |> Expect.equal (Just Square)
        ,  test "should return a triangle for a pryamid" <|
            \_ -> 
                removeForDoubleShapes (Extrusion Triangle Triangle) |> Expect.equal (Just Triangle)
        ,  test "should return nothing for a prism" <|
            \_ -> 
                (removeForDoubleShapes (Extrusion Square Triangle), removeForDoubleShapes (Extrusion Triangle Square)) |> Expect.equal (Nothing, Nothing)
        ,  test "should return nothing for a cone" <|
            \_ -> 
                (removeForDoubleShapes (Extrusion Circle Triangle), removeForDoubleShapes (Extrusion Triangle Circle)) |> Expect.equal (Nothing, Nothing)
        ,  test "should return nothing for a cylinder" <|
            \_ -> 
                (removeForDoubleShapes (Extrusion Square Circle), removeForDoubleShapes (Extrusion Circle Square)) |> Expect.equal (Nothing, Nothing)
        ]