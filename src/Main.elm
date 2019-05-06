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
        conversations = Data.conversations |> List.map ConversationCard
        locations = Data.locations |> List.map LocationCard
        bugs = Data.bugConversation |> List.map BugCard
        cards = List.concat [ conversations, locations ]
        cards1 = subList 0 12 cards 
        cards2 = subList 12 24 cards 
        cards3 = subList 24 36 cards 
    in
  { pages = 
    {cards = bugs |> List.indexedMap createFrontFace } 
    :: {cards = bugs |> List.map BackFace } 
    :: {cards = cards1 |> List.indexedMap createFrontFace }
    :: {cards = cards1 |> List.map BackFace }
    :: {cards = cards2 |> List.indexedMap createFrontFace }
    :: {cards = cards2 |> List.map BackFace }
    :: {cards = cards3 |> List.indexedMap createFrontFace }
    :: {cards = cards3 |> List.map BackFace }
    :: []
  }

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


createFrontFace : Int -> Card -> CardFace
createFrontFace order card =
    FrontFace ((order % Data.numCardBorders) + 1) card
