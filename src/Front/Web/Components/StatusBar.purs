module Web.Components.StatusBar where

import Prelude


import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Noodle.Id (LinkR) as Id
import Noodle.Raw.Link (Connector) as RawLink
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette

import Data.Text.Format (Tag) as T

import Web.Paths (statusBar) as P


type Input =
    { content :: T.Tag
    , width :: Number
    }


type State =
    { content :: T.Tag
    , width :: Number
    }


data Action
    = Receive Input


type Output
    = Unit


height = 25.0


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
initialState { content, width } = { content, width }


render :: forall m. State -> H.ComponentHTML Action () m
render state =
    HS.path
        [ HSA.d $ P.statusBar { slope : slopeFactor, height, width : state.width }
        , HSA.fill $ Just $ P.hColorOf $ _.i800 Palette.yellow
        , HSA.stroke $ Just $ P.hColorOf $ _.i800 Palette.yellow
        ]
    where
        slopeFactor = 5.0


handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
    Receive { content, width } -> H.put { content, width }