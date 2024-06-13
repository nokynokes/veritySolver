module Shapes exposing (..)

type Shape2D = Circle | Square | Triangle 
type Shape3D 
    = Sphere 
    | Cube
    | Pyramid
    | Cylinder
    | Cone
    | Prism

combine : Shape2D -> Shape2D -> Shape3D
combine shape1 shape2 = 
    case (shape1, shape2) of
        (Circle, Circle) -> Sphere
        (Square, Square) -> Cube
        (Triangle, Triangle) -> Pyramid 
        (Circle, Square) -> Cylinder
        (Square, Circle) -> Cylinder
        (Circle, Triangle) -> Cone
        (Triangle, Circle) -> Cone
        (Square, Triangle) -> Prism
        (Triangle, Square) -> Prism

subtract : Shape2D -> Shape3D -> Shape2D
subtract dissectShape shape = 
    case (dissectShape, shape) of
        (Circle, Sphere) -> Circle
        (Square, Cube) -> Square
        (Triangle, Pyramid) -> Triangle
        (Circle, Cylinder) -> Square
        (Square, Cylinder) -> Circle
        (Circle, Cone) -> Triangle
        (Triangle, Cone) -> Circle
        (Square, Prism) -> Triangle
        (Triangle, Prism) -> Square
        _ -> dissectShape