module Shapes exposing
    ( Shape2D(..)
    , Shape3D(..)
    , combine
    , hasShapes
    , isComplete
    , removeForDoubleShapes
    , shapesMissing
    , subtract
    , toString2D
    , toString3D
    , isSphere
    , isCube
    , isPyramid
    , isCylinder
    , isCone
    , isPrism
    )

import List.Extra exposing (find)

type Shape2D
    = Circle
    | Square
    | Triangle

type Shape3D
    = Extrusion Shape2D Shape2D

{-| Helper functions to check what type of 3D shape we have -}
isSphere : Shape3D -> Bool
isSphere (Extrusion shape1 shape2) =
    shape1 == Circle && shape2 == Circle


isCube : Shape3D -> Bool
isCube (Extrusion shape1 shape2) =
    shape1 == Square && shape2 == Square


isPyramid : Shape3D -> Bool
isPyramid (Extrusion shape1 shape2) =
    shape1 == Triangle && shape2 == Triangle


isCylinder : Shape3D -> Bool
isCylinder (Extrusion shape1 shape2) =
    (shape1 == Circle && shape2 == Square) || (shape1 == Square && shape2 == Circle)


isCone : Shape3D -> Bool
isCone (Extrusion shape1 shape2) =
    (shape1 == Circle && shape2 == Triangle) || (shape1 == Triangle && shape2 == Circle)


isPrism : Shape3D -> Bool
isPrism (Extrusion shape1 shape2) =
    (shape1 == Square && shape2 == Triangle) || (shape1 == Triangle && shape2 == Square)

{-| Create a 3D shape from two 2D shapes -}
combine : Shape2D -> Shape2D -> Shape3D
combine shape1 shape2 =
    Extrusion shape1 shape2


subtract : Shape2D -> Shape3D -> Maybe Shape2D
subtract dissectShape (Extrusion face1 face2) =
    if dissectShape == face1 then
        Just face2
    else if dissectShape == face2 then
        Just face1
    else
        Nothing

type alias ShapeMappings = 
    { checkForShape3D : Shape3D -> Bool
    , getMissingShapes2D : Shape2D -> List Shape2D
    }

missingShapeMappings : List ShapeMappings
missingShapeMappings =
    [ { checkForShape3D = isCube
      , getMissingShapes2D = \shape -> case shape of
            Square -> [Triangle, Circle]
            Circle -> [Triangle]
            Triangle -> [Circle]
      }
    , { checkForShape3D = isSphere
      , getMissingShapes2D = \shape -> case shape of
            Square -> [Triangle]
            Circle -> [Square, Triangle]
            Triangle -> [Square]
      }
    , { checkForShape3D = isPyramid
      , getMissingShapes2D = \shape -> case shape of
            Square -> [Circle]
            Circle -> [Square]
            Triangle -> [Circle, Square]
      }
    ]


shapesMissing : Shape2D -> Shape3D -> Maybe (List Shape2D)
shapesMissing insideShape outsideShape =
    if isPrism outsideShape && insideShape /= Circle then
        Just [ Circle ]
    else if isCylinder outsideShape && insideShape /= Triangle then
        Just [ Triangle ]
    else if isCone outsideShape && insideShape /= Square then
        Just [ Square ]
    else 
        missingShapeMappings
            |> find (\mapping -> mapping.checkForShape3D outsideShape)
            |> Maybe.map (\mapping -> mapping.getMissingShapes2D insideShape)

hasShapes : List Shape2D -> Shape3D -> Bool
hasShapes shapes outsideShape =
    List.map (hasShape outsideShape) shapes
        |> List.foldl (||) False


hasShape : Shape3D -> Shape2D -> Bool
hasShape outsideShape insideShape =
    case subtract insideShape outsideShape of
        Just _ ->
            True
        _ ->
            False

removeForDoubleShapes : Shape3D -> Maybe Shape2D
removeForDoubleShapes (Extrusion shape1 shape2) =
    if shape1 == shape2 then
        Just shape1
    else
        Nothing

toString2D : Shape2D -> String
toString2D shape =
    case shape of
        Circle ->
            "Circle"
        Square ->
            "Square"
        Triangle ->
            "Triangle"

{-| Convert a Shape3D to its canonical name -}
toString3D : Shape3D -> String
toString3D shape =
    if isSphere shape then
        "Sphere"
    else if isCube shape then
        "Cube"
    else if isPyramid shape then
        "Pyramid"
    else if isCylinder shape then
        "Cylinder"
    else if isCone shape then
        "Cone"
    else if isPrism shape then
        "Prism"
    else
        "Unknown Shape"


isComplete : Shape2D -> Shape3D -> Bool
isComplete inside (Extrusion outside1 outside2) =
    inside /= outside1 && inside /= outside2
