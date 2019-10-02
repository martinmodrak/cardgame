module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Data


view : Model -> Html Msg
view model =
    div [ Attr.class "wrapper" ] 
    (List.map viewPage model.pages |> List.concat) 

-- credits : List (Html Msg)
-- credits = 
--     [ p [] [ text "Cube image from https://commons.wikimedia.org/wiki/File:Cube_(PSF).png" ]
--     ]

viewPage : Page -> List (Html Msg)
viewPage page =
    [ div [ Attr.class "page"] (List.map viewCard page.cards)
    , div [ Attr.class "pagebreak"] [] ]

viewCard : Card -> Html Msg
viewCard card = 
    let 
        content = 
            case card of
                ActivityCard activity -> viewActivity activity
    in 
        div [ Attr.class "card"] [ content ]

viewActivity : Activity -> Html Msg
viewActivity activity =
    let 
        content =             
            ( div [ Attr.class "name"] [ text activity.name ] ) ::
            (activity.skills |> List.map viewSkill)            
    in
        div [ Attr.class "Activity"] content

viewSkill : (Int, Direction) -> Html Msg
viewSkill (skill, direction) =
    div [ Attr.classList [("skill", True), (classForDirection direction, True)]] [ text (toString skill) ]

classForDirection : Direction -> String
classForDirection direction =
    case direction of
        Left -> "left"
        Top -> "top"
        Bottom -> "bottom"
        Right -> "right"