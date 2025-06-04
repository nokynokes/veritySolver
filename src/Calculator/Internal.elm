module Calculator.Internal exposing (compareStatues)

import Shapes exposing (Shape2D(..), Shape3D(..))
import Statues exposing (Statue)
import Statues.Internal exposing (Position)
import Statues.Internal exposing (Position(..))

numberOfStepsToCompelete : Shape2D -> Shape3D -> Int
numberOfStepsToCompelete insideShape (Extrusion shape1 shape2) =
    if insideShape == shape1 && insideShape == shape2 then
        2
    else
        1

positionOrder : Position -> Int
positionOrder pos = 
    case pos of
        Left -> 3
        Middle -> 2
        Right -> 1


compareStatues : Statue -> Statue -> Order
compareStatues s1 s2 =
    let
        steps1 =
            numberOfStepsToCompelete s1.insideShape s1.outsideShape

        steps2 =
            numberOfStepsToCompelete s2.insideShape s2.outsideShape

        position1 = positionOrder s1.position
        position2 = positionOrder s2.position

        stepsCompare = compare steps1 steps2
    in
    case stepsCompare of
        EQ -> compare position2 position1
        _ -> stepsCompare
