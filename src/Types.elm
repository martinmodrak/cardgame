module Types exposing (..)

type Topic 
    = Family 
    | Work
    | Hobbies
    | People

type EffectTarget 
    = You
    | Friend
    | YouAndFriend    

type alias Effect = 
    { target : EffectTarget
    , focus : Int
    , cards : Int
    , conditional: Maybe { focus : Int, cards: Int}
    }

type alias Conversation =
    { topic : Topic
    , depth : Int
    , name : String
    , effect : Effect
    , followups: List Topic
    }

type alias Location =
    { name : String
    , effect : Effect
    }

type Card 
    = BugCard Conversation
    | ConversationCard Conversation 
    | LocationCard Location

type CardFace
    = BlankFace
    | BackFace Card
    | FrontFace Int Card

type alias Page =
    { cards: List CardFace
    }


type alias Model =
    { pages : List Page
    }
