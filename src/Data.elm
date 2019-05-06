module Data exposing(..)

import Types exposing (..)

cardsPerPage : Int
cardsPerPage = 12

conditionalEffectBound: Int
conditionalEffectBound = 2

numCardBorders : Int
numCardBorders = 3

locations : List Location
locations =
    [ Location "" {target = Friend, focus = 1 , cards = 0, conditional = Just { focus = 2, cards = 0}} 
    , Location "" {target = Friend, focus = 1 , cards = 1, conditional = Just { focus = 2, cards = 1}} 
    , Location "" (simpleFriendEffect 1 0)
    , Location "" (simpleFriendEffect 1 1)
    , Location "" {target = YouAndFriend, focus = 1 , cards = 0, conditional = Nothing} 
    ]

conversations : List Conversation
conversations =
    [ depth1Conversations 
    , depth2Conversations 
    , depth3Conversations ] |> List.concat

depth1Conversations: List Conversation
depth1Conversations =
    [ cardSet 1 Family 
        [ ("Mom's birthday"
          , {target = You, focus = 1 , cards = 0, conditional = Just { focus = 3, cards = 0}} 
          , Right)
        ,("Dinner with in-laws", simpleFriendEffect 0 2, Both)
        ]
    , cardSet 1 Work 
        [ ("Asking for a rise"
          , {target = You, focus = 1 , cards = 0, conditional = Just { focus = 2, cards = 0}} 
          , Left)
        ,("New project", simpleFriendEffect 0 1, Both)
        ]
    , cardSet 1 Hobbies 
        [ ("A new board game"
          , {target = You, focus = 0 , cards = 1, conditional = Just { focus = 2, cards = 1}} 
          , Left)
        ,("Something I read online", simpleFriendEffect 0 2, Both)
        ]
    , cardSet 1 People
        [ ("A friend has a new job"
          , {target = You, focus = 0 , cards = 1, conditional = Just { focus = 1, cards = 2}} 
          , Right)
        ,("", simpleFriendEffect 0 1, Both)
        ]
    ] |> List.concat

depth2Conversations: List Conversation
depth2Conversations =
    [ cardSet 2 Family 
        [ ("", simpleEffect 3 0, Left)
        ,("", simpleEffect 2 0, Both)
        ]
    , cardSet 2 Work 
        [ ("", simpleEffect 2 1, Right)
        ,("", simpleEffect 1 1, Both)
        ]
    , cardSet 2 Hobbies 
        [ ("", simpleEffect 2 0, Right)
        ,("", simpleEffect 1 1, Both)
        ]
    , cardSet 2 People
        [ ("", simpleEffect 2 1, Left)
        ,("", simpleEffect 2 0, Both)
        ]
    ] |> List.concat

depth3Conversations: List Conversation
depth3Conversations =
    [ cardSet 3 Family 
        [ ("", simpleEffect 1 1, Left)
        ,("", simpleEffect 2 0, Right)
        ]
    , cardSet 3 Work 
        [ ("", simpleEffect 2 0, Left)
        ,("", simpleEffect 1 2, Right)
        ]
    , cardSet 3 Hobbies 
        [ ("", simpleEffect 2 0, Left)
        ,("", simpleEffect 0 3, Right)
        ]
    , cardSet 3 People
        [ ("", simpleEffect 1 2, Left)
        ,("", simpleEffect 2 2, Right)
        ]
    ] |> List.concat

bugConversation: List Conversation
bugConversation =
    [ cardSet 4 Family 
        [ ("", noEffect, No)
        , ("", noEffect, No)
        ]
    , cardSet 4 Work 
        [ ("", noEffect, No)
        , ("", noEffect, No)
        ]
    , cardSet 4 Hobbies 
        [ ("", noEffect, No)
        , ("", noEffect, No)
        ]
    , cardSet 4 People
        [ ("", noEffect, No)
        , ("", noEffect, No)
        ]
    ] |> List.concat

noEffect: Effect 
noEffect = 
    { target = You
    , focus = 0
    , cards = 0
    , conditional = Nothing
    }

simpleEffect: Int -> Int -> Effect
simpleEffect focus cards = 
    { target = You
    , focus = focus
    , cards = cards
    , conditional = Nothing
    }

simpleFriendEffect: Int -> Int -> Effect
simpleFriendEffect focus cards = 
    { target = Friend
    , focus = focus
    , cards = cards
    , conditional = Nothing
    }

cardSet: Int -> Topic -> List (String, Effect, Adjacency) -> List Conversation
cardSet depth topic defList =
    defList |> List.map (cardFromDef depth topic)

cardFromDef: Int -> Topic -> (String, Effect, Adjacency) -> Conversation
cardFromDef depth topic (name, effect, adjacency) =
    { depth = depth
    , topic = topic
    , name = name
    , effect = effect
    , followups = followupsFromAdjacency topic adjacency
    }


followupsFromAdjacency : Topic -> Adjacency -> List Topic
followupsFromAdjacency topic adjacency =
    let 
        tail = case adjacency of
            Left -> [ adjacentLeft topic]
            Right -> [adjacentRight topic ]
            Both -> [adjacentLeft topic, adjacentRight topic]
            No -> []
    in 
        topic :: tail

type Adjacency 
    = Left
    | Right
    | Both
    | No

adjacentLeft : Topic -> Topic
adjacentLeft topic =
    case topic of
        Family -> Work
        Work -> Hobbies
        Hobbies -> People
        People -> Family

adjacentRight : Topic -> Topic
adjacentRight topic =
    case topic of
        Family -> People
        Work -> Family
        Hobbies -> Work
        People -> Hobbies