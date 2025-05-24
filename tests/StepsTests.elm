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

validateSteps : Step -> StepValidation -> StepValidation -> Expectation
validateSteps (step1, step2) expected1 expected2 = 
    if validateStep step1 expected1 && validateStep step2 expected2 then
        Expect.pass
    else
        Expect.fail "Step did not match expected transformation"


singleStep : (Statue, Statue) -> (StepValidation, StepValidation) -> Expectation
singleStep (statue1, statue2) (expected1, expected2) =
    case generateStep statue1 statue2 of
        Just step -> validateSteps step expected1 expected2
        Nothing -> Expect.fail "It should have generated a step"

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
        
statuesOrdered1 : List Statue
statuesOrdered1 = 
    [   { insideShape = Square, outsideShape = Prism, position = Middle }
    ,   { insideShape = Triangle, outsideShape = Prism, position = Right }
    ,   { insideShape = Circle, outsideShape = Sphere, position = Left }
    ]

statuesOrdered2 : List Statue
statuesOrdered2 = 
    [   { insideShape = Circle, outsideShape = Sphere, position = Left }
    ,   { insideShape = Square, outsideShape = Cube, position = Middle }
    ,   { insideShape = Triangle, outsideShape = Pyramid, position = Right }
    ]

generateStepsTest : Test
generateStepsTest = 
    describe "Generate Steps btwn all three statues" 
        [   describe "in exactly two steps"
                [   test "should dissect btwn middle and left first then right and left"
                        (\_ -> 
                            let 
                                steps = generateSteps statuesOrdered1 
                            in
                                case steps of
                                    [step1, step2] -> 
                                        let
                                            (statue1, statue2) = step1
                                            (statue11, statue22) = step2
                                        in
                                            case ((statue1.statueAfterDissect.position, statue1.shapeToDissect, statue1.statueAfterDissect.outsideShape), (statue2.statueAfterDissect.position, statue2.shapeToDissect, statue2.statueAfterDissect.outsideShape)) of 
                                                ((Middle, Square, Cone), (Left, Circle, Cylinder)) -> 
                                                    case ((statue11.statueAfterDissect.position, statue11.shapeToDissect, statue11.statueAfterDissect.outsideShape), (statue22.statueAfterDissect.position, statue22.shapeToDissect, statue22.statueAfterDissect.outsideShape)) of
                                                        ((Right, Triangle, Cylinder), (Left, Circle, Prism)) -> Expect.pass
                                                        _ -> Expect.fail "it should dissect a triangle off of right and cirlce off of left"
                                                _ -> Expect.fail "it should dissect a square off of middle and cirlce off of left"
                                    _ -> Expect.fail "it should generate only 2 steps"
                        )

                ]
        ,   describe "in exactly three steps"
                [   test "should dissect btwn Left and Middle first then middle and right"
                        (\_ -> 
                            let
                                steps = generateSteps statuesOrdered2
                            in
                                case steps of
                                    [step1, step2 , step3] -> 
                                        let
                                            (statue1, statue2) = step1
                                            (statue11, statue22) = step2
                                            (statue111, statue222) = step3
                                        in
                                            case ((statue1.statueAfterDissect.position, statue1.shapeToDissect, statue1.statueAfterDissect.outsideShape), (statue2.statueAfterDissect.position, statue2.shapeToDissect, statue2.statueAfterDissect.outsideShape)) of
                                                ((Left, Circle, Cylinder), (Middle, Square, Cylinder)) -> 
                                                    case ((statue11.statueAfterDissect.position, statue11.shapeToDissect, statue11.statueAfterDissect.outsideShape), (statue22.statueAfterDissect.position, statue22.shapeToDissect, statue22.statueAfterDissect.outsideShape)) of
                                                        ((Left, Circle, Prism), (Right, Triangle, Cone)) -> 
                                                            case ((statue111.statueAfterDissect.position, statue111.shapeToDissect, statue111.statueAfterDissect.outsideShape), (statue222.statueAfterDissect.position, statue222.shapeToDissect, statue222.statueAfterDissect.outsideShape)) of
                                                                ((Middle, Square, Cone), (Right, Triangle, Cylinder)) -> Expect.pass
                                                                _ -> Expect.fail "it should dissect a sqaure off of middle and a triangle off of right"
                                                        _ -> Expect.fail "it should dissect a circle off of left and a triangle off of right"
                                                _ -> Expect.fail "it should dissect a circle off of left and square off of middle"
                                        
                                    _ -> Expect.fail "it should generate only 3 steps"

                            
                        )
                ]

        ]