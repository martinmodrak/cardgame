module Data exposing(..)

import Types exposing (..)
import Names
import Random
import RandomUtils

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

cardsGenerator : Random.Generator (List Card)
cardsGenerator =
    let
        sheepCards =  Random.map (List.map SheepCard) (generateForAllTeams 1 sheepGenerator)
        activityCards = Random.map (List.map ActivityCard) (generateForAllTeams 1 activitiesGenerator)
    in
        Random.map2 List.append  sheepCards activityCards

generateForAllTeams : Int -> (Int -> Random.Generator (List a)) -> Random.Generator (List a)
generateForAllTeams team rawGenerator =
    if team >= numTeams then
        rawGenerator team
    else 
        Random.map2 List.append (rawGenerator team) (generateForAllTeams (team + 1) rawGenerator)  

activitiesGenerator : Int -> Random.Generator (List Activity)
activitiesGenerator team =
    directionsGenerator |> Random.list (List.length activitiesData) |>
    Random.map (\listDirections -> List.map2 (activityFromData team) activitiesData listDirections)

sheepGenerator : Int -> Random.Generator (List Sheep)
sheepGenerator team =
    let 
        needsListGenerator = needsGenerator |> Random.list (List.length sheepData) 
        directionsListGenerator = directionsGenerator |> Random.list (List.length sheepData)
        names = Names.namesForTeam team
        imagesListGenerator = imagesGenerator sheepImages |> Random.list (List.length sheepData)
    in
        Random.map3  (\listDirections needs images -> List.map5 (sheepFromData team) names needs sheepData listDirections images) directionsListGenerator needsListGenerator imagesListGenerator


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
    , ("Vaření", [2])
    , ("Deskovky", [3])
    , ("Kytary", [4])
    , ("Pokec u ohně", [5])
    --------
    , ("Menší ORWO", [1, 2])
    , ("Povídání o emocích", [1, 3])
    , ("Služba lidem", [1, 4])
    , ("Služba přírodě", [1, 5])
    , ("Služba památce", [2, 3])
    , ("Putování", [2, 4])
    , ("Duchovní večer", [2, 5])
    , ("Krátký psycho program", [3, 4])
    , ("Workshop na téma", [3, 5])
    , ("Sdílecí program", [4, 5])
    ------
    , ("LARP", [1, 2, 3])
    , ("Šifrovačka", [1, 2, 4])
    , ("Velká terénní hra", [1, 2, 5])
    , ("Zdravotnická simulačka", [1, 3, 4])
    , ("Velká simulační hra", [1, 3, 5])
    , ("Akce pro veřejnost", [1, 4, 5])
    , ("Velký psycho program", [2, 3, 4])
    , ("Vymazlené ORWO", [2, 3, 5])
    , ("Komponovaný večer", [2, 4, 5])
    , ("Silný zážitkový", [3, 4, 5])
    ]

needsGenerator : Random.Generator (List (Need, Int))
needsGenerator =
    RandomUtils.rejectionSampling validNeeds needsGeneratorRaw |>
    Random.map (\x -> List.map2 (,) [Posun, Pratelstvi, Presah] x)


needsGeneratorRaw : Random.Generator (List Int)
needsGeneratorRaw =
    let 
        numToSplit = Random.int 6 8
        splits = numToSplit |> Random.andThen (splitGenerator 3)
    in
        splits |> Random.map (List.map ((+)2))

validNeeds : List Int -> Bool
validNeeds needs =
    case needs of 
        [] -> True
        head :: tail -> if head <= 6 then validNeeds tail else False

splitGenerator : Int -> Int -> Random.Generator (List Int)
splitGenerator numCategories numItems  =
    let
        breaksRaw = Random.int 0 numItems |> Random.list (numCategories - 1)
        breaksSorted = Random.map List.sort breaksRaw
    in
        Random.map (breaksToSplits numItems 0) breaksSorted

breaksToSplits : Int -> Int -> List Int -> List Int
breaksToSplits numItems previousSplit breaks =
    case breaks of
        [] -> [numItems - previousSplit]
        head :: tail -> (head - previousSplit) :: (breaksToSplits numItems head tail)

sheepData : List (List Int)
sheepData = 
    [ [1, 1]
    , [2, 2]
    , [3, 3]
    , [4, 4]
    , [1, 1, 2]
    , [1, 2, 2]
    , [1, 1, 3]
    , [1, 3, 3]
    , [1, 1, 4]
    , [1, 4, 4]
    , [2, 2, 3]
    , [2, 3, 3]
    , [2, 2, 4]
    , [2, 4, 4]
    , [3, 3, 4]
    , [3, 4, 4]
    , [1, 1, 2, 3]
    , [1, 2, 2, 4]
    , [1, 3, 3, 4]
    , [2, 3, 4, 4]
    ]

imagesGenerator : List (Bool, String, String, List String) -> Random.Generator (List SheepImage)
imagesGenerator imageSet =
    case imageSet of
        [] -> RandomUtils.fixedGenerator []
        (color, class, first,others) :: tail -> (RandomUtils.listMemberGenerator first others) |> 
            Random.andThen (\x -> imagesGenerator tail |> Random.map (\y -> {class = class, doColor = color, name = x} :: y)  )

sheepImages: List (Bool, String, String, List String)
sheepImages =
    [ (False, "nohy", "nohy-01.svg", [])
    , (False, "telo", "telo1-01.svg", ["telo2-01.svg","telo3-01.svg","telo4-01.svg"])
    , (False, "hlava", "hlava-01.svg", [])
    , (False, "ocas", "ocas1-01.svg", ["ocas2-01.svg", ""])
    , (True, "oci", "oci1-01.svg", ["oči 2-01.svg"])
    , (True, "pusa", "pusa1-01.svg", ["pusa2-01.svg", "pusa2-01.svg"])
    , (True, "usi", "usi1-01.svg", ["usi2-01.svg"])
    ]

sheepFromData : Int -> String -> List (Need, Int) -> List Int -> List Direction -> List SheepImage -> Sheep
sheepFromData team name needs rawSkills directions images =
    let
        omittedSkill = omittedSkillForTeam team
        skills = List.map (rawSkillsToSkills omittedSkill) rawSkills        
    in  
        { name = name
        , team = team
        , needs = needs
        , images = images
        , skills =  List.map2 (,) skills directions
        }

omittedSkillForTeam : Int -> Int
omittedSkillForTeam team =
    ((team - 1) % numSkills) + 1
 
rawSkillsToSkills : Int -> Int -> Int
rawSkillsToSkills omittedSkill rawSkill =
    if rawSkill < omittedSkill then
        rawSkill
    else
        rawSkill + 1

activityFromData : Int -> (String, List Int) -> (List Direction) -> Activity
activityFromData team (name, skills) directions =
    { name = name
    , team = team
    , skills =  List.map2 (,) skills directions
    }
    
nextDirection : Direction -> Direction
nextDirection direction =
    case direction of
        Top -> Right
        Right -> Bottom
        Bottom -> Left
        Left -> Top