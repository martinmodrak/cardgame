module RandomUtils exposing (
        fixedGenerator
        , rejectionSampling
        , listMemberGenerator
    )

import Random
import Utils

fixedGenerator : a -> Random.Generator a
fixedGenerator value =
    --I do not consume the bool, but there is no way to create my own primitive generator
    Random.map (\_ -> value) Random.bool

rejectionSampling : (a -> Bool) -> Random.Generator a -> Random.Generator a
rejectionSampling check generator =
    generator |> Random.andThen (\x -> if (check x) then fixedGenerator x else rejectionSampling check generator)


--explicit head to always have something to return
listMemberGenerator : a -> List a -> Random.Generator a
listMemberGenerator listHead listTail =
    let
        numElements =
            (List.length listTail) + 1
    in
        Random.int 0 (numElements - 1)
            |> Random.map
                (\index ->
                    if index == 0 then
                        listHead
                    else
                        case Utils.listGet (index - 1) listTail of
                            Just x ->
                                x

                            Nothing ->
                                listHead
                )
