module Main exposing (..)

import Types exposing (..)
import View
import Html
import Data 
import Random


main =
    Html.program
        { init = ({pages = [] }, Random.generate (Generated) (Data.cardsGenerator))
        , view = View.view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        Generated cards ->
            ({ pages = splitToPages Data.cardsPerPage cards
            }, Cmd.none)

splitToPages : Int -> List Card -> List Page
splitToPages cardsPerPage cards =
    case cards of
        head :: tail ->
            let
                (takenCards, tail) = takeN cardsPerPage cards
            in 
                (Page takenCards) :: (splitToPages cardsPerPage tail)
        [] -> []

takeN : Int -> List a -> (List a, List a)
takeN numRemaining l =
    if numRemaining <= 0 then 
        ([], l)
    else 
        case l of 
            head :: tail ->
                let 
                    (taken, returnTail) = takeN (numRemaining - 1) tail
                in 
                    (head :: taken, returnTail)
            [] ->
                ([], [])

subList : Int -> Int -> List a -> List a
subList start end l =
    if start > 0 then
        case l of 
            head :: tail -> subList (start - 1) (end - 1) tail
            [] -> []
    else if end == 0 then
        []
    else
        case l of 
            head :: tail -> head :: (subList 0 (end - 1) tail)
            [] -> []
