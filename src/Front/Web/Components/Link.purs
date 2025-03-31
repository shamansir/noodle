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


type Input =
    { id :: Id.LinkR
    , connector :: RawLink.Connector
    , position ::
        { from :: { x :: Number, y :: Number }
        , to   :: { x :: Number, y :: Number }
        }
    }


type State =
    { id :: Id.LinkR
    , connector :: RawLink.Connector
    , position ::
        { from :: { x :: Number, y :: Number }
        , to   :: { x :: Number, y :: Number }
        }
    }


type Action
    = Unit


type Output
    = Unit


component :: forall query m. H.Component query Input Output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            }
        }


initialState :: Input -> State
initialState { id, connector, position } = { id, connector, position }


render :: forall m. State -> H.ComponentHTML Action () m
render state =
    HS.line
        [ HSA.x1 state.position.from.x
        , HSA.y1 state.position.from.y
        , HSA.x2 state.position.to.x
        , HSA.y2 state.position.to.y
        , HSA.stroke $ Just $ P.hColorOf $ _.i200 Palette.magenta
        , HSA.strokeWidth 2.0
        ]


handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
    _ -> pure unit
