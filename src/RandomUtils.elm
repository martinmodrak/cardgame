module RandomUtils exposing (
        fixedGenerator
        , rejectionSampling
    )

import Random

fixedGenerator : a -> Random.Generator a
fixedGenerator value =
    --I do not consume the bool, but there is no way to create my own primitive generator
    Random.map (\_ -> value) Random.bool

rejectionSampling : (a -> Bool) -> Random.Generator a -> Random.Generator a
rejectionSampling check generator =
    generator |> Random.andThen (\x -> if (check x) then fixedGenerator x else rejectionSampling check generator)


--explicit head to always have something to return


