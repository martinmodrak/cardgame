module Data exposing(..)

import Types exposing (..)
import Random

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

activitiesGenerator : Random.Generator (List Activity)
activitiesGenerator =
    directionsGenerator |> Random.list (List.length activitiesData) |>
    Random.map (\listDirections -> List.map2 activityFromData activitiesData listDirections)

directionsGenerator : Random.Generator (List Direction)
directionsGenerator =
    Random.map directionsFromInt (Random.int 1 24)

directionsFromInt : Int -> List Direction
directionsFromInt i =
    case i of
        1 ->  [Top, Left, Bottom, Right]
        2 ->  [Top, Left, Right, Bottom]
        3 ->  [Top, Bottom, Left, Right]
        4 ->  [Top, Bottom, Right, Left]
        5 ->  [Top, Right, Bottom, Left]
        6 ->  [Top, Right, Left, Bottom]
        7 ->  [Left, Top, Bottom, Right]
        8 ->  [Left, Top, Right, Bottom]
        9 ->  [Left, Bottom, Top, Right]
        10 -> [Left, Bottom, Right, Top]
        11 -> [Left, Right, Bottom, Top]
        12 -> [Left, Right, Top, Bottom]
        13 -> [Bottom, Top, Left, Right]
        14 -> [Bottom, Top, Right, Left]
        15 -> [Bottom, Left, Top, Right]
        16 -> [Bottom, Left, Right, Top]
        17 -> [Bottom, Right, Left, Top]
        18 -> [Bottom, Right, Top, Left]
        19 -> [Right, Top, Bottom, Left]
        20 -> [Right, Top, Left, Bottom]
        21 -> [Right, Bottom, Top, Left]
        22 -> [Right, Bottom, Left, Top]
        23 -> [Right, Left, Bottom, Top]
        24 -> [Right, Left, Top, Bottom]
        _ -> []

activitiesData : List (String, List Int)
activitiesData =
    [ ("Výlet", [1])
    , ("2", [2])
    , ("3", [3])
    , ("4", [4])
    , ("5", [5])
    ------------- 0 means
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

activityFromData : (String, List Int) -> (List Direction) -> Activity
activityFromData (name, skills) directions =
    { name = name
    , skills = List.map2 (,) skills directions
    }
    
nextDirection : Direction -> Direction
nextDirection direction =
    case direction of
        Top -> Right
        Right -> Bottom
        Bottom -> Left
        Left -> Top