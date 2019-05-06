module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Data


view : Model -> Html Never
view model =
    div [ Attr.class "wrapper" ] 
    (List.append (List.map viewPage model.pages) )

-- credits : List (Html Never)
-- credits = 
--     [ p [] [ text "Cube image from https://commons.wikimedia.org/wiki/File:Cube_(PSF).png" ]
--     ]

viewPage : Page -> List (Html Never)
viewPage page =
    [ div [ Attr.class "page"] (List.map viewCardFace page.cards)
    , div [ Attr.class "pagebreak"] [] ]

viewCardFace : CardFace -> Html Never
viewCardFace face = 
    let 
        content = case face of
            BlankFace -> [] 
            BackFace card -> [ viewBackFace card ]
            FrontFace borderId card -> 
                [ img [Attr.class "fullCardImg"
                    , Attr.src (String.concat ["img/card", toString borderId, ".svg"]) ] []  
                , viewFrontFace card ]
    in 
    div [ Attr.class "card"] content

viewFrontFace : Card -> Html Never
viewFrontFace card  =
    case card of 
        ConversationCard convo ->
            viewConversation convo
        BugCard convo ->
            viewConversation convo
        LocationCard location ->
            viewLocation location

viewBackFace : Card -> Html Never 
viewBackFace card =
    let 
        imageName = case card of 
            BugCard _ -> "bugBack"
            _ -> "back"
    in
    img [Attr.class "fullCardImg"
         , Attr.src (String.concat ["img/", imageName, ".svg"]) ] []  


viewLocation : Location -> Html Never
viewLocation location =
    let 
        content =             
            [ div [ Attr.class "name"] [ text location.name ]
            , div [ Attr.class "locationTag"] [ text "Location" ]
            , div [ Attr.class "effect"] (viewEffect location.effect)
            , div [ Attr.class "effectText"] [ text (effectText location.effect)]            
            ]
    in
        div [ Attr.class "location"] content


viewConversation : Conversation -> Html Never
viewConversation convo =
    let 
        mainContent = 
            [ img [ Attr.class "topic", Attr.src (imgForTopic convo.topic) ] []
            , div [ Attr.class "depth"] [ text (toString convo.depth)]
            , div [ Attr.class "name"] [ text convo.name]
            , div [ Attr.class "effect"] (viewEffect convo.effect)
            , div [ Attr.class "effectText"] [ text (effectText convo.effect)]            
            ]
        followups = [ div [ Attr.class "followups"] (List.map viewFollowup convo.followups) ]
    in
        div [ Attr.class "conversation"] (List.append mainContent followups)

viewEffect : Effect -> List (Html Never)
viewEffect effect = 
    let
        target = case effect.target of 
            You -> []
            Friend -> [ img [Attr.class "targetFriend", Attr.src "img/friend.svg" ] []]
            YouAndFriend -> [ img [Attr.class "targetBoth", Attr.src "img/targetBoth.svg" ] []]
        core = viewEffectCore effect.focus effect.cards
        conditional = case effect.conditional of 
            Nothing -> []
            Just condCore -> [ div [Attr.class "conditionalEffect"] (viewConditionalEffect condCore.focus condCore.cards) ]
    in
        List.concat [ target, core, conditional]

viewConditionalEffect : Int -> Int -> List (Html Never)    
viewConditionalEffect focus cards =
    (img [Attr.class "conditionalEffectHeader", Attr.src "img/conditionalEffect.svg"] [] ) 
    :: (text "≤ ")       
    :: (text (toString Data.conditionalEffectBound) )
    :: (img [Attr.class "conditionalEffectBridge", Attr.src "img/conditionalEffectBridge.svg"] [] ) 
    :: (viewEffectCore focus cards)

viewEffectCore : Int -> Int -> List (Html Never)    
viewEffectCore n_focus n_cards =
    let 
        focus = List.repeat n_focus (img [Attr.class "gainFocus", Attr.src "img/focus2.svg"] [])
        cards = List.repeat n_cards (img [Attr.class "gainCard", Attr.src "img/card.svg"] [])
    in
        List.append focus cards



viewFollowup : Topic -> Html Never
viewFollowup topic = 
    img [Attr.class "followup", Attr.src (imgForTopic topic)] []

imgForTopic : Topic -> String
imgForTopic topic =
    let 
        name = case topic of
                Family -> "family"
                Hobbies -> "hobbies"
                Work -> "work"
                People -> "people"
    in
        String.concat ["img/", name, ".svg"]    

effectText : Effect -> String 
effectText effect =
    let
        target = case effect.target of
                    You -> " you "
                    Friend -> " your friend "
                    YouAndFriend -> " both players "
        gain = case effect.target of
                    You -> " gain "
                    Friend -> " gains "
                    YouAndFriend -> " gain "
        core = effectTextCore effect.focus effect.cards
        main = if effect.focus > 0 || effect.cards > 0 then 
            String.concat 
            [ target 
            , gain
            , core
            , ". "
            ] else ""
        conditional = case effect.conditional of 
            Nothing -> ""
            Just condCore -> String.concat 
                [ " If "
                , target
                , " have ≤ " 
                , toString Data.conditionalEffectBound
                , " focus, "
                , target
                , gain
                , effectTextCore condCore.focus condCore.cards
                , " instead."
                ]        
    in
        String.concat 
            [ main
            , conditional
            ]
    
effectTextCore : Int -> Int -> String
effectTextCore focus cards =
    if focus > 0 then
        if cards > 0 then 
            String.concat [ toString focus, " focus and ", toString cards, " cards"]
        else 
            String.concat [ toString focus, " focus"]
    else 
        if cards > 0 then 
            String.concat [ toString cards, " cards"]
        else 
            ""