module Rpd.Renderer.Html.DebugBox
    ( Model, init, update, view )
    where

import Prelude (Unit)

import Data.List

import Spork.Html (Html)
import Spork.Html as H

import Rpd.Command as C
import Rpd.Network as R


type Model =
    { lastMessages :: forall d. List (C.Command d)
    }


init :: Model
init =
    { lastMessages : Nil
    }


update :: forall d. C.Command d -> R.Network d -> Model -> Model
update cmd nw model = model


view :: forall d. R.Network d -> Model -> Html Unit
view nw model =
    H.div [] []
