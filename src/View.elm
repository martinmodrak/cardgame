module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Names
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
        (content, class) = 
            case card of
                ActivityCard activity -> (viewActivity activity, "activity")
                SheepCard sheep -> (viewSheep sheep, "sheep")
    in 
        div [ Attr.classList [("card", True), (class, True)]] [ content ]

viewActivity : Activity -> Html Msg
viewActivity activity =
    let 
        content =             
            ( img [ Attr.class "background", Attr.src "img/milion/pozadi-01.svg", Attr.style [("filter", "saturate(0) brightness(1.06)")]] []) ::
            ( div [ Attr.class "name"] [ text activity.name ] ) ::
            ( div [ Attr.class "letter"] [ text (Names.teamToLetter activity.team)]) :: 
            (activity.skills |> List.map viewSkill)            
    in
        div [ Attr.class "content"] content

viewSheep : Sheep -> Html Msg
viewSheep sheep =
    let 
        content =             
            ( img [ Attr.class "background", Attr.src "img/milion/pozadi-01.svg", Attr.style (styleForTeam sheep.team)] []) ::
            ( div [ Attr.class "name"] [ text sheep.name ] ) ::
            ( img [ Attr.class "needsText", Attr.src "img/milion/3P-01.svg"] []) ::
            ( div [ Attr.class "visual"] (sheep.images |> List.map (viewSheepImage sheep.team))) ::
            (List.append 
                (sheep.skills |> List.map viewSkill)            
                
                (sheep.needs |> List.map viewNeed)            
            )
    in
        div [ Attr.class "content"] content

viewSheepImage: Int -> SheepImage -> Html Msg 
viewSheepImage team image =
    let
        style = if image.doColor then styleForTeam team else []  
    in
        if image.name == "" then 
            text "" 
        else 
            img [ Attr.src ("img/milion/" ++ image.name), Attr.class image.class, Attr.style style ] []

styleForTeam: Int -> List(String, String)
styleForTeam team = 
    [("filter", filterForTeam team)]

filterForTeam: Int -> String
filterForTeam team =
    let 
        (hue, saturate) =
            case team of
                1 -> (0, 100)
                2 -> (215, 100)
                _ -> (0, 0)
    in
        "hue-rotate(" ++ toString(hue) ++ "deg) saturate(" ++ toString(saturate) ++ "%)"

viewSkill : (Int, Direction) -> Html Msg
viewSkill (skill, direction) =
    div [ Attr.classList [("skill", True), (classForDirection direction, True)]] [ img [ Attr.src ("img/milion/dovednost" ++ (toString skill) ++ "-01.png") ] [] ]

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


