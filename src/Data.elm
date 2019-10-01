module Data exposing(..)

import Types exposing (..)

cardsPerPage : Int
cardsPerPage = 20

sheepsPerTeam : Int
sheepsPerTeam = 20

numSkills : Int
numSkills = 5

numSkillsSingleTeam : Int
numSkillsSingleTeam = 4

numTeams : Int
numTeams = 7

activities : List Activity
activities = activitiesData |> List.indexedMap activityFromDataWrapper

activitiesData : List (String, List Int)
activitiesData =
    [ ("Výlet", [1])
    , ("2", [2])
    , ("3", [3])
    , ("4", [4])
    , ("5", [5])
    -------------
    , ("Menší ORWO", [1, 2])
    , ("2", [1, 3])
    , ("3", [1, 4])
    , ("4", [1, 5])
    , ("5", [2, 3])
    , ("6", [2, 4])
    , ("7", [2, 5])
    , ("8", [3, 4])
    , ("9", [3, 5])
    , ("10", [4, 5])
    ------
    , ("Menší ORWO", [1, 2, 3])
    , ("2", [1, 2, 4])
    , ("3", [1, 2, 5])
    , ("4", [1, 3, 4])
    , ("5", [1, 3, 5])
    , ("6", [1, 4, 5])
    , ("7", [2, 3, 4])
    , ("8", [2, 3, 5])
    , ("9", [2, 4, 5])
    , ("10", [3, 4, 5])
    ]

activityFromDataWrapper : Int -> (String, List Int) -> Activity 
activityFromDataWrapper order data =
    let
        flipOrder = (order % 2 == 1)
        startDirection = 
            Left
    in
        activityFromData data startDirection flipOrder

activityFromData : (String, List Int) -> Direction -> Bool -> Activity
activityFromData (name, skills) startDirection flipOrder =
    { name = name
    , skills = skillsFromData (if flipOrder then List.reverse skills else skills) startDirection
    }

skillsFromData : List Int -> Direction -> List (Int, Direction)
skillsFromData skills startDirection =
    case skills of 
        [] -> []
        head :: tail -> 
            (head, startDirection) :: (skillsFromData tail (nextDirection startDirection))
    
nextDirection : Direction -> Direction
nextDirection direction =
    case direction of
        Top -> Right
        Right -> Bottom
        Bottom -> Left
        Left -> Top