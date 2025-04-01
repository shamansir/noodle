module Web.Components.Link where

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


type Position =
    { from :: { x :: Number, y :: Number }
    , to   :: { x :: Number, y :: Number }
    }


type Input =
    { id :: Id.LinkR
    , connector :: RawLink.Connector
    , position :: Position
    }


type State =
    { id :: Id.LinkR
    , connector :: RawLink.Connector
    , position :: Position
    }


data Action
    = Receive Input


type Output
    = Unit


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
initialState { id, connector, position } = { id, connector, position }


render :: forall m. State -> H.ComponentHTML Action () m
render state =
    linkShape state.position


handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
    Receive { id, connector, position } -> H.put { id, connector, position }


linkShape :: forall action slots m. Position -> H.ComponentHTML action slots m
linkShape position =
    HS.line
        [ HSA.x1 position.from.x
        , HSA.y1 position.from.y
        , HSA.x2 position.to.x
        , HSA.y2 position.to.y
        , HSA.stroke $ Just $ P.hColorOf $ _.i200 Palette.magenta
        , HSA.strokeWidth 2.0
        ]