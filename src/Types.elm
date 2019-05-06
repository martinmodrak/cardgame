module Types exposing (..)

import Html

type Card 
    = BlankCard    

type alias Page
    = List


type alias Model =
    { pages : List Page
    }
