module App.Component.Patch where


import Prelude (identity, (<<<), Void)

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))

import Noodle.Patch as Noodle
import Noodle.Toolkit.Shaped as Noodle
import Noodle.Toolkit.Shaped as Toolkit


import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS

import Type.Proxy (Proxy(..))


type Slot id = forall query. H.Slot query Void id


type Input d =
    { patch :: Noodle.Patch d Unit
    , toolkit :: Noodle.Toolkit d
    }


type State d =
    { patch :: Noodle.Patch d Unit
    , toolkit :: Noodle.Toolkit d
    }


data Action d
    = Receive (Input d)
    | AddNode String


initialState :: forall d. Input d -> State d
initialState = identity


render :: forall d m. State d -> H.ComponentHTML (Action d) () m
render _ = HS.svg [] []


handleAction :: forall output m d. Action d -> H.HalogenM (State d) (Action d) () output m Unit
handleAction = case _ of
  Receive input ->
    H.modify_ (\state -> state { patch = input.patch })
  AddNode name ->
    H.modify_ (\state -> state)


component :: forall query output m d. H.Component query (Input d) output m
component =
    H.mkComponent
        { initialState
        , render
        , eval:
            H.mkEval H.defaultEval
                { handleAction = handleAction
                , receive = Just <<< Receive
                }
        }