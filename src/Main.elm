module Main exposing (..)

import Types exposing (..)
import View
import Html
import Data
import String.Normalize
import List.Extra


main =
    Html.beginnerProgram
        { model = createModel
        , view = View.view
        , update = update
        }


createModel : Model
createModel =
  {pages = []}


update : Never -> Model -> Model
update _ model =
    model
