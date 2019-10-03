module Utils exposing (listGet)

import String


listGet : Int -> List a -> Maybe a
listGet index list =
    if index < 0 then
        Nothing
    else
        case list of
            head :: tail ->
                if index == 0 then
                    Just head
                else
                    listGet (index - 1) tail

            [] ->
                Nothing


