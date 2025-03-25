module Web.Components.Library where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Array (snoc) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (length) as String
import Data.Int (toNumber) as Int
import Data.Foldable (foldl)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Noodle.Id (FamilyR, family) as Id

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette


type Input =
    { families :: Array Id.FamilyR
    }


type State =
    { families :: Array Id.FamilyR
    }


data Action
    = Initialize
    | RaiseSelectFamily Id.FamilyR
    | Receive Input


data Output
    = SelectFamily Id.FamilyR


component :: forall query m. H.Component query Input Output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
        }


initialState :: Input -> State
initialState { families } = { families }


render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HS.g
    [ HSA.transform [ HSA.Translate 0.0 70.0 ] ]
    $ familyButton <$> state.families
    where
        familyButton familyR =
            HS.g
                []
                [ HS.text
                    [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.blue ]
                    [ HH.text $ Id.family familyR
                    ]
                ]


handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
    Initialize -> pure unit
    RaiseSelectFamily familyR -> do
        H.raise $ SelectFamily familyR
    Receive input ->
        H.modify_ _
            { families = input.families
            }
