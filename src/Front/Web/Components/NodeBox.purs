module Web.Components.NodeBox where

import Prelude


import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Array ((:))
import Data.Array (snoc) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (length) as String
import Data.Int (toNumber) as Int
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Noodle.Id (FamilyR, family, familyOf) as Id
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id) as RawNode

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette


type Input sterpr chrepr m =
    { node :: Raw.Node sterpr chrepr m
    }


type State sterpr chrepr m =
    { node :: Raw.Node sterpr chrepr m
    }


data Action sterpr chrepr m
    = Initialize
    | Receive (Input sterpr chrepr m)


data Output
    = Output


component :: forall query m sterpr chrepr mi. H.Component query (Input sterpr chrepr mi) Output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
        }


initialState :: forall sterpr chrepr mi. Input sterpr chrepr mi -> State sterpr chrepr mi
initialState { node } = { node }


render :: forall m sterpr chrepr mi. State sterpr chrepr mi -> H.ComponentHTML (Action sterpr chrepr mi) () m
render state =
    HS.g
        [ HSA.transform [ HSA.Translate 200.0 80.0 ] ]
        [ HS.text
            [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.blue ]
            [ HH.text $ Id.family $ Id.familyOf $ RawNode.id state.node ]  ]


handleAction :: forall m sterpr chrepr mi. Action sterpr chrepr mi -> H.HalogenM (State sterpr chrepr mi) (Action sterpr chrepr mi) () Output m Unit
handleAction = case _ of
    Initialize -> pure unit
    Receive input ->
        H.modify_ _
            { node = input.node
            }
