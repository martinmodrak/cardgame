module Main exposing (..)

import Types exposing (..)
import View
import Html
import Data 

main =
    Html.beginnerProgram
        { model = createModel
        , view = View.view
        , update = \_ model -> model
        }


createModel : Model
createModel =
    let 
        activities = Data.activities |> List.map ActivityCard
        cards = List.concat [ activities ]
    in
  { pages = splitToPages Data.cardsPerPage cards
  }

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
