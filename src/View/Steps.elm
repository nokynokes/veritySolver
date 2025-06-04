module View.Steps exposing (renderSteps)

import Html.Styled as Html exposing (Html, div, h1, h2, hr, text)
import Html.Styled.Attributes as Html exposing (css, step)
import Msg exposing (Msg)
import Shapes exposing (toString2D, toString3D, isComplete)
import Statues.Internal exposing (Position(..), toString)
import Steps.Internal exposing (StatueDissect, Step)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


type StepType 
    = DissectStep 
    | AfterDissectStep

statueName : StatueDissect -> Html Msg
statueName dissect = 
    let 
        name = toString dissect.statueAfterDissect.position 
    in
        div [ css [ Tw.text_3xl, Tw.pb_3 ] ] [ text <| "On the " ++ String.toLower name ++ " statue:" ]

stepForStatue : StatueDissect -> Html Msg
stepForStatue dissect =
    let
        dissectShape =
            toString2D dissect.shapeToDissect
    in
    Html.div
        [ css [ Tw.text_color Theme.white ] ]
        [ statueName dissect
        , div [ css [ Tw.text_xl, Tw.py_3 ] ]
              [ Html.p [] [ text <| "Dissect a " ++ String.toLower dissectShape ] ]
        ]

afterDissect : StatueDissect -> Html Msg
afterDissect dissect =
    let
        afterDissectShape =
            toString3D dissect.statueAfterDissect.outsideShape
    in
    Html.div
        [ css [ Tw.text_color Theme.white ] ]
        [ statueName dissect
        , div [ css [ Tw.text_xl, Tw.py_3 ] ]
              [ Html.p [] [ text <| "There should be a " ++ afterDissectShape ]
              , if isComplete dissect.statueAfterDissect.insideShape dissect.statueAfterDissect.outsideShape then
                  Html.p [] [ text "Statue is done!" ]
                else
                  Html.p [] [ text "" ]
              ]
        ]

renderStep : StepType -> Step -> Html Msg
renderStep stepType ( statue1, statue2 ) =
    div [ css [ Tw.flex, Tw.flex_row, Tw.gap_64, Bp.xxl [ Tw.justify_around ], Bp.xl [ Tw.justify_around ], Bp.lg [ Tw.justify_around ], Tw.justify_between ] ]
        <| case stepType of
             DissectStep -> [ stepForStatue statue1, stepForStatue statue2 ]
             AfterDissectStep -> [ afterDissect statue1, afterDissect statue2 ]

renderDissectStep : Step -> Html Msg
renderDissectStep =
    renderStep DissectStep

renderShapesAfterDissect : Step -> Html Msg
renderShapesAfterDissect =
    renderStep AfterDissectStep

renderSteps_ : Int -> Step -> Html Msg
renderSteps_ stepNumber step = 
    div [ css [ Tw.bg_color Theme.zinc_700, Tw.border_solid, Tw.border_2, Tw.border_color Theme.slate_400, Tw.rounded, Tw.px_10 ] ]
        [ h1 [ css [ Tw.text_color Theme.white ] ] [ text <| "Step " ++ String.fromInt (stepNumber + 1) ]
        , renderDissectStep step
        , hr [] []
        , h2 [ css [ Tw.text_color Theme.white ] ] [ text "After dissection: " ]
        , renderShapesAfterDissect step
        ]

renderSteps : List Step -> List (Html Msg)
renderSteps =
    List.indexedMap renderSteps_
