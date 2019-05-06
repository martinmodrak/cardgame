module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import List.Extra
import Data


view : Model -> Html Never
view model =
    div [ Attr.class "wrapper" ] (List.map viewPage model.pages |> List.concat)


viewPage : Page -> List (Html Never)
viewPage page =
    []



