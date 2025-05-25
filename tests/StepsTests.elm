module StepsTests exposing (..)

import Expect exposing (..)
import Test exposing (..)

import Shapes exposing(Shape2D(..), Shape3D(..))
import Statues.Internal exposing (Position(..))
import Statues exposing (Statue)
import Steps.Internal exposing (Step, StatueDissect, generateStep)
import Steps exposing (generateSteps)


type alias StepValidation = 
    { position : Position
    , outsideShape : Shape3D
    , shapeToDissect : Shape2D
    }

validateStep : StatueDissect -> StepValidation -> Bool
validateStep step expected =
    step.statueAfterDissect.position == expected.position
        && step.statueAfterDissect.outsideShape == expected.outsideShape
        && step.shapeToDissect == expected.shapeToDissect

validateSteps : (Step, (StepValidation, StepValidation)) -> Expectation
validateSteps ((step1, step2), (expected1, expected2)) = 
    if validateStep step1 expected1 && validateStep step2 expected2 then
        Expect.pass
    else
        Expect.fail "Step did not match expected transformation"


singleStep : (Statue, Statue) -> (StepValidation, StepValidation) -> Expectation
singleStep (statue1, statue2) (expected1, expected2) =
    case generateStep statue1 statue2 of
        Just step -> validateSteps (step, (expected1, expected2))
        Nothing -> Expect.fail "It should have generated a step"

manySteps : List Statue -> List (StepValidation, StepValidation) -> Expectation
manySteps statuesOrdered expectedSteps  =
    case generateSteps statuesOrdered of
        [] -> Expect.fail "It should have generated some amount of steps"
        steps -> 
            if List.length steps /= List.length expectedSteps then
                Expect.fail "It should have generated the same amount of expected steps"
            else
                let
                    zippedList = List.map2 Tuple.pair steps expectedSteps
                in
                    List.foldl 
                        (\step acc -> 
                            if acc == Expect.pass then
                                validateSteps step
                            else
                                acc
                        ) 
                        Expect.pass 
                        zippedList
        
generateStatue : Position -> Shape2D -> Shape3D -> Statue
generateStatue pos shape2d shape3d = 
    { position = pos
    , insideShape = shape2d
    , outsideShape = shape3d
    }

generateExpectedStep : Position -> Shape3D -> Shape2D -> StepValidation
generateExpectedStep pos outsideShape shapeToDissect =
    { position = pos
    , outsideShape = outsideShape
    , shapeToDissect = shapeToDissect
    }

generateSingleStepTests : Test
generateSingleStepTests =
    describe "Generate a single step btwn to statues"
        [   test "should swap Square with Circle when single shapes" <|
                \_ -> 
                    let
                        statue1 = generateStatue Middle Square Prism
                        statue2 = generateStatue Left Circle Cone
                        expectedStep1 = generateExpectedStep Middle Cone Square
                        expectedStep2 = generateExpectedStep Left Prism Circle
                    in
                       singleStep (statue1, statue2) (expectedStep1, expectedStep2)
            , test "should swap Triangle with Circle when single shapes" <|
                \_ -> 
                    let
                        statue1 = generateStatue Right Triangle Prism
                        statue2 = generateStatue Left Circle Sphere
                        expectedStep1 = generateExpectedStep Right Cylinder Triangle
                        expectedStep2 = generateExpectedStep Left Cone Circle
                    in
                        singleStep (statue1, statue2) (expectedStep1, expectedStep2)
            , test "should not be able to generate a step" <|
                \_ -> 
                    let
                        statue1 = generateStatue Middle Square Cone
                        statue2 = generateStatue Left Circle Cone
                    in
                        case generateStep statue1 statue2 of
                            Nothing -> Expect.pass
                            _ -> Expect.fail "It should not be able to subtract a square from a cone"
            , test "should swap Square with Circle" <|
                \_ -> 
                    let
                        statue1 = generateStatue Left Circle Cube
                        statue2 = generateStatue Right Square Pyramid
                        expectedStep1 = generateExpectedStep Left Prism Square
                        expectedStep2 = generateExpectedStep Right Prism Triangle
                    in
                       singleStep (statue1, statue2) (expectedStep1, expectedStep2)
            , test "should swap Square with Circle when inside shapes are same as double shapes" <|
                \_ -> 
                    let
                        statue1 = generateStatue Left Circle Sphere
                        statue2 = generateStatue Middle Square Cube
                        expectedStep1 = generateExpectedStep Left Cylinder Circle
                        expectedStep2 = generateExpectedStep Middle Cylinder Square
                    in
                        singleStep (statue1, statue2) (expectedStep1, expectedStep2)
            ]
        

generateStepsTest : Test
generateStepsTest = 
    describe "Generate Steps between all three statues" 
        [   describe "in exactly two steps"
                [   test "should dissect between middle and left first then right and left"
                        (\_ -> 
                            let 
                                statuesOrdered = 
                                    [   generateStatue Middle Square Prism
                                    ,   generateStatue Right Triangle Prism
                                    ,   generateStatue Left Circle Sphere
                                    ]

                                expectedSteps1 = 
                                    (   generateExpectedStep Middle Cone Square
                                    ,   generateExpectedStep Left Cylinder Circle
                                    )
                                expectedSteps2 =
                                    (   generateExpectedStep Right Cylinder Triangle
                                    ,   generateExpectedStep Left Prism Circle
                                    )
                                expectedSteps = [ expectedSteps1, expectedSteps2 ]
                            in
                                manySteps statuesOrdered expectedSteps
                        )

                ]
        ,   describe "in exactly three steps"
                [   test "should dissect btwn Left and Middle first then middle and right"
                        (\_ -> 
                            let
                                statuesOrdered = 
                                    [   generateStatue Left Circle Sphere
                                    ,   generateStatue Middle Square Cube
                                    ,   generateStatue Right Triangle Pyramid
                                    ]
                                expectedSteps1 = 
                                    (   generateExpectedStep Left Cylinder Circle
                                    ,   generateExpectedStep Middle Cylinder Square    
                                    )
                                expectedSteps2 =
                                    (   generateExpectedStep Left Prism Circle
                                    ,   generateExpectedStep Right Cone Triangle
                                    )
                                expectedSteps3 =
                                    (   generateExpectedStep Middle Cone Square
                                    ,   generateExpectedStep Right Cylinder Triangle
                                    )
                                expectedSteps = [ expectedSteps1, expectedSteps2, expectedSteps3 ]
                            in
                                manySteps statuesOrdered expectedSteps                            
                        )
                ]

        ]