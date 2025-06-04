module StepsTests exposing 
    ( generateSingleStepTests
    , generateStepsTest
    )

import Expect exposing (..)
import Test exposing (..)

import Shapes exposing(Shape2D(..), Shape3D(..))
import Statues.Internal exposing (Position(..))
import Statues exposing (Statue)
import Steps.Internal exposing (Step, StatueDissect, generateStep)
import Steps exposing (generateSteps)
import Shapes exposing (isCone, isCube, isCylinder, isPrism, isSphere, isPyramid)

type alias StepValidation = 
    { position : Position
    , checkOutsideShape : Shape3D -> Bool
    , shapeToDissect : Shape2D
    }

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

validateStep : StatueDissect -> StepValidation -> Bool
validateStep step expected =
    step.statueAfterDissect.position == expected.position
        && expected.checkOutsideShape step.statueAfterDissect.outsideShape
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
                    validator = 
                        (\step acc -> 
                            if acc == Expect.pass then
                                validateSteps step
                            else
                                acc
                        ) 
                in
                    List.foldl validator (Expect.fail "Invalid step") zippedList
        
generateStatue : Position -> Shape2D -> Shape3D -> Statue
generateStatue pos shape2d shape3d = 
    { position = pos
    , insideShape = shape2d
    , outsideShape = shape3d
    }

generateExpectedStep : Position -> (Shape3D -> Bool) -> Shape2D -> StepValidation
generateExpectedStep pos checker shapeToDissect =
    { position = pos
    , checkOutsideShape = checker
    , shapeToDissect = shapeToDissect
    }

generateSingleStepTests : Test
generateSingleStepTests =
    describe "Generate a single step btwn to statues"
        [   test "should swap Square with Circle when single shapes" <|
                \_ -> 
                    let
                        statue1 = generateStatue Middle Square prism
                        statue2 = generateStatue Left Circle cone
                        expectedStep1 = generateExpectedStep Middle isCone Square
                        expectedStep2 = generateExpectedStep Left isPrism Circle
                    in
                       singleStep (statue1, statue2) (expectedStep1, expectedStep2)
            , test "should swap Triangle with Circle when single shapes" <|
                \_ -> 
                    let
                        statue1 = generateStatue Right Triangle prism
                        statue2 = generateStatue Left Circle sphere
                        expectedStep1 = generateExpectedStep Right isCylinder Triangle
                        expectedStep2 = generateExpectedStep Left isCone Circle
                    in
                        singleStep (statue1, statue2) (expectedStep1, expectedStep2)
            , test "should not be able to generate a step" <|
                \_ -> 
                    let
                        statue1 = generateStatue Middle Square cone
                        statue2 = generateStatue Left Circle cone
                    in
                        case generateStep statue1 statue2 of
                            Nothing -> Expect.pass
                            _ -> Expect.fail "It should not be able to subtract a square from a cone"
            , test "should swap Square with Circle" <|
                \_ -> 
                    let
                        statue1 = generateStatue Left Circle cube
                        statue2 = generateStatue Right Square pyramid
                        expectedStep1 = generateExpectedStep Left isPrism Square
                        expectedStep2 = generateExpectedStep Right isPrism Triangle
                    in
                       singleStep (statue1, statue2) (expectedStep1, expectedStep2)
            , test "should swap Square with Circle when inside shapes are same as double shapes" <|
                \_ -> 
                    let
                        statue1 = generateStatue Left Circle sphere
                        statue2 = generateStatue Middle Square cube
                        expectedStep1 = generateExpectedStep Left isCylinder Circle
                        expectedStep2 = generateExpectedStep Middle isCylinder Square
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
                                    [   generateStatue Middle Square prism
                                    ,   generateStatue Right Triangle prism
                                    ,   generateStatue Left Circle sphere
                                    ]

                                expectedSteps1 = 
                                    (   generateExpectedStep Middle isCone Square
                                    ,   generateExpectedStep Left isCylinder Circle
                                    )
                                expectedSteps2 =
                                    (   generateExpectedStep Right isCylinder Triangle
                                    ,   generateExpectedStep Left isPrism Circle
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
                                    [   generateStatue Left Circle sphere
                                    ,   generateStatue Middle Square cube
                                    ,   generateStatue Right Triangle pyramid
                                    ]
                                expectedSteps1 = 
                                    (   generateExpectedStep Left isCylinder Circle
                                    ,   generateExpectedStep Middle isCylinder Square    
                                    )
                                expectedSteps2 =
                                    (   generateExpectedStep Left isPrism Circle
                                    ,   generateExpectedStep Right isCone Triangle
                                    )
                                expectedSteps3 =
                                    (   generateExpectedStep Middle isCone Square
                                    ,   generateExpectedStep Right isCylinder Triangle
                                    )
                                expectedSteps = [ expectedSteps1, expectedSteps2, expectedSteps3 ]
                            in
                                manySteps statuesOrdered expectedSteps                            
                        )
                ]

        ]