module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Data


view : Model -> Html Msg
view model =
    div [ Attr.class "wrapper" ] 
    (List.map viewPage model.pages |> List.concat) 


viewPage : Page -> List (Html Msg)
viewPage page =
    [ div [ Attr.class "page"] (List.map viewCard page.cards)
    , div [ Attr.class "pagebreak"] [] ]

viewCard : Card -> Html Msg
viewCard card = 
    let 
        (content, class, team) = 
            case card of
                ActivityCard activity -> (viewActivity activity, "activity", activity.team)
                SheepCard sheep -> (viewSheep sheep, "sheep", sheep.team)
    in 
        div [ Attr.classList [("card", True), (class, True)], Attr.style [("background-color", teamToColor team)]] [ content ]

viewActivity : Activity -> Html Msg
viewActivity activity =
    let 
        content =             
            ( div [ Attr.class "name"] [ text activity.name ] ) ::
            (activity.skills |> List.map viewSkill)            
    in
        div [ Attr.class "content"] content

viewSheep : Sheep -> Html Msg
viewSheep sheep =
    let 
        content =             
            ( div [ Attr.class "name"] [ text sheep.name ] ) ::
            (List.append 
                (sheep.skills |> List.map viewSkill)            
                
                (sheep.needs |> List.map viewNeed)            
            )
    in
        div [ Attr.class "content"] content

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

viewNeed : (Need, Int) -> Html Msg
viewNeed (need, number) = 
    div [ Attr.classList [("need", True), (classForNeed need, True)]] [ 
        div [ Attr.class "needLabel" ] [ text (textForNeed need)],
        div [ Attr.class "needValue" ] [ text (toString number) ]
        ]

classForNeed : Need -> String
classForNeed need =
    case need of
        Pratelstvi -> "pratelstvi"
        Posun -> "posun"
        Presah -> "presah"

textForNeed : Need -> String
textForNeed need =
    case need of
        Pratelstvi -> "Přátelství"
        Posun -> "Posun"
        Presah -> "Přesah"

teamToColor : Int -> String
teamToColor team =
    case team of
        1 -> "lightblue"
        2 -> "#ffc0c0"
        3 -> "lightgreen"
        4 -> "gold"
        5 -> "lightcyan"
        6 -> "#ffc0ff"
        7 -> "lightyellow"
        _ -> "gray"