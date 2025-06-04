module Statues exposing (Statue)

import Shapes exposing (Shape2D(..), Shape3D(..))
import Statues.Internal exposing (Position)


type alias Statue =
    { position : Position
    , insideShape : Shape2D
    , outsideShape : Shape3D
    }

   