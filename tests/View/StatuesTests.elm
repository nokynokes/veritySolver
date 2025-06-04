module View.StatuesTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import View.Statues exposing (checkLimits)
import Shapes exposing (Shape2D(..), Shape3D(..))

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

checkLimitsTests : Test
checkLimitsTests =
    describe "checkLimits" 
        [ test "should hit limit when sphere is selected" <|
            \_ -> Expect.equal True (checkLimits [sphere] sphere)
        , test "should hit limit when sphere is selected again" <|
            \_ -> Expect.equal True (checkLimits [sphere, cone] sphere)
        , test "should hit limit when cube is selected" <|
            \_ -> Expect.equal True (checkLimits [cube] cube)
        , test "should hit limit when cube is selected again" <|
            \_ -> Expect.equal True (checkLimits [cube, prism] cube)
        , test "should hit limit when pyramid is selected" <|
            \_ -> Expect.equal True (checkLimits [pyramid] pyramid)
        , test "should hit limit when pyramid is selected again" <|
            \_ -> Expect.equal True (checkLimits [pyramid, prism] pyramid)
        , test "should hit limit when cone is selected" <|
            \_ -> Expect.equal True (checkLimits [sphere] cone)
        , test "should hit limit when cone is selected again" <|
            \_ -> Expect.equal True (checkLimits [pyramid] cone)
        , test "should hit limit when cone is selected again again" <|
            \_ -> Expect.equal True (checkLimits [sphere, cylinder] cone)
        , test "should hit limit when cone is selected again again again" <|
            \_ -> Expect.equal True (checkLimits [pyramid, prism] cone)
        , test "should hit limit when cylinder is selected" <|
            \_ -> Expect.equal True (checkLimits [sphere] cylinder)
        , test "should hit limit when cylinder is selected again" <|
            \_ -> Expect.equal True (checkLimits [cube] cylinder)
        , test "should hit limit when cylinder is selected again again" <|
            \_ -> Expect.equal True (checkLimits [sphere, cone] cylinder)
        , test "should hit limit when cylinder is selected again again again" <|
            \_ -> Expect.equal True (checkLimits [cube, prism] cylinder)
        , test "should hit limit when prism is selected" <|
            \_ -> Expect.equal True (checkLimits [pyramid] prism)
        , test "should hit limit when prism is selected again" <|
            \_ -> Expect.equal True (checkLimits [cube] prism)
        , test "should hit limit when prism is selected again again" <|
            \_ -> Expect.equal True (checkLimits [pyramid, cone] prism)
        , test "should hit limit when prism is selected again again again" <|
            \_ -> Expect.equal True (checkLimits [cube, cylinder] prism)
        ]