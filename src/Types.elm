module Types exposing (..)

type Need 
    = Pratelstvi 
    | Posun
    | Presah

type Direction 
    = Top
    | Left
    | Right
    | Bottom

type alias Sheep =
    { name: String
    , needs: List (Need, Int)
    , skills: List (Int, Direction)
    }

type alias Activity =
    { name: String
    , skills: List (Int, Direction)
    }

type Card 
    --= SheepCard Sheep
    = ActivityCard Activity 

type alias Page =
    { cards: List Card
    }

type Msg =
    Generated (List Activity)


type alias Model =
    { pages : List Page
    }
