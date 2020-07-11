module UI.Component
    ( Component, Component'
    , class RendersTo, render
    , make, makePassing
    ) where


import Prelude (($))

import Data.List (List)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)

import UI (UI)
import UI (once, make, makePassing) as UI


data Component' action model view = Component' model (UI action model view)


type Component action model view = Component' action model (view action)


class RendersTo model view where
    render :: model -> view


instance rendersToUi :: RendersTo (Component' action model view) view where
    render (Component' model ui) = UI.once ui model


make
    :: forall model action view
     . (action -> model -> model /\ List (Effect action))
    -> (model -> view action)
    -> model
    -> Component action model view
make updateF viewF model =
    Component' model $ UI.make updateF viewF


makePassing
    :: forall model action view
     . (model -> view action)
    -> model
    -> Component action model view
makePassing viewF model =
    Component' model $ UI.makePassing viewF
