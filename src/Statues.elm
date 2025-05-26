module Statues exposing (Statue, isComplete)

import Shapes exposing (Shape2D(..), Shape3D(..))
import Statues.Internal exposing (Position)


type alias Statue =
    { position : Position
    , insideShape : Shape2D
    , outsideShape : Shape3D
    }


isComplete : Shape2D -> Shape3D -> Bool
isComplete inside (Extrusion outside1 outside2) =
    inside /= outside1 && inside /= outside2
   