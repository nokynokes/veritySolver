module Model exposing 
    ( Model
    , StatueSelection
    , ensureNoIllegalSelections
    , initModel
    , selected2Dshapes
    , selected3Dshapes
    , solveShapes
    )

import Calculator exposing (orderToSolve)
import Shapes exposing (Shape2D(..), Shape3D(..), isComplete)
import Statues exposing (Statue)
import Statues.Internal exposing (Position(..))
import Steps exposing (generateSteps)
import Steps.Internal exposing (Step)


type alias StatueSelection =
    { insideShape : Maybe Shape2D
    , outsideShape : Maybe Shape3D
    }


type alias Model =
    { leftStatueSelections : StatueSelection
    , middleStatueSelections : StatueSelection
    , rightStatueSelections : StatueSelection
    , steps : List Step
    }

emptySelection : StatueSelection
emptySelection =
    { insideShape = Nothing, outsideShape = Nothing }

initModel : Model
initModel =
    { leftStatueSelections = emptySelection, middleStatueSelections = emptySelection, rightStatueSelections = emptySelection, steps = [] }

selected2Dshapes : Model -> List Shape2D
selected2Dshapes model =
    selectedShapes [ model.leftStatueSelections.insideShape, model.middleStatueSelections.insideShape, model.rightStatueSelections.insideShape ]

selected3Dshapes : Model -> List Shape3D
selected3Dshapes model =
    selectedShapes [ model.leftStatueSelections.outsideShape, model.middleStatueSelections.outsideShape, model.rightStatueSelections.outsideShape ]

selectedShapes : List (Maybe s) -> List s
selectedShapes shapes =
    List.filterMap identity shapes

selectionToStatue : ( Position, StatueSelection ) -> Statue
selectionToStatue ( pos, selection ) =
    { position = pos
    , insideShape = Maybe.withDefault Circle selection.insideShape
    , outsideShape = Maybe.withDefault (Extrusion Circle Circle) selection.outsideShape
    }


solveShapes : Model -> Model
solveShapes model =
    let
        insideSelections =
            (selected2Dshapes >> List.length) model

        outsideSelections =
            (selected3Dshapes >> List.length) model
    in
    if insideSelections /= 3 || outsideSelections /= 3 then
        { model | steps = [] }

    else
        let
            statues =
                List.map selectionToStatue <|
                    List.map2 Tuple.pair
                        [ Left, Middle, Right ]
                        [ model.leftStatueSelections, model.middleStatueSelections, model.rightStatueSelections ]
        in
        { model
            | steps = (orderToSolve >> generateSteps) statues
        }


ensureNoIllegalSelections : Model -> Model
ensureNoIllegalSelections model =
    { model
        | leftStatueSelections =
            Maybe.map2 isComplete model.leftStatueSelections.insideShape model.leftStatueSelections.outsideShape
                |> Maybe.withDefault False
                |> ensureOutsideSelection model.leftStatueSelections
        , middleStatueSelections =
            Maybe.map2 isComplete model.middleStatueSelections.insideShape model.middleStatueSelections.outsideShape
                |> Maybe.withDefault False
                |> ensureOutsideSelection model.middleStatueSelections
        , rightStatueSelections =
            Maybe.map2 isComplete model.rightStatueSelections.insideShape model.rightStatueSelections.outsideShape
                |> Maybe.withDefault False
                |> ensureOutsideSelection model.rightStatueSelections
    }


ensureOutsideSelection : StatueSelection -> Bool -> StatueSelection
ensureOutsideSelection selection condition =
    if condition then
        { selection | outsideShape = Nothing }

    else
        selection
