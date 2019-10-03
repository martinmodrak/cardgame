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
    , team: Int
    , needs: List (Need, Int)
    , skills: List (Int, Direction)
    }

type alias Activity =
    { name: String
    , team: Int
    , skills: List (Int, Direction)
    }

type Card 
    = SheepCard Sheep
    | ActivityCard Activity 

type alias Page =
    { cards: List Card
    }

type Msg =
    Generated (List Card)


type alias Model =
    { pages : List Page
    }
