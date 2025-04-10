module Web.Components.Link where

import Prelude

import Data.Maybe (Maybe(..))

import Record as Record

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HHP
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
    , handleEvents :: Boolean
    }


type State =
    { id :: Id.LinkR
    , connector :: RawLink.Connector
    , position :: Position
    , handleEvents :: Boolean
    }


data Action
    = Receive Input
    | Skip
    | Clicked


data Output
    = WasClicked


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
initialState { id, connector, position, handleEvents } = { id, connector, position, handleEvents }


render :: forall m. State -> H.ComponentHTML Action () m
render state =
    HS.g
        [ HE.onClick $ const $ if state.handleEvents then Clicked else Skip
        , HHP.class_ $ H.ClassName $ if state.handleEvents then "noodle-enable-events" else "noodle-disable-events"
        ]
        [ linkShape { strokeWidth : 5.0, strokeColor : P.transparent, handleEvents : state.handleEvents } state.position -- so that clickable area is wider
        , linkShape { strokeWidth : 2.0, strokeColor : _.i200 Palette.magenta, handleEvents : state.handleEvents  } state.position
        ]


handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
    Receive { id, connector, position, handleEvents } ->
        H.modify_ _ { id = id, connector = connector, position = position, handleEvents = handleEvents }
    Skip -> pure unit
    Clicked -> H.raise WasClicked


linkShapeNotYetConnected :: forall action slots m. Position -> H.ComponentHTML action slots m
linkShapeNotYetConnected = linkShape { strokeWidth : 2.0, strokeColor : _.i200 Palette.magenta, handleEvents : false }


linkShape :: forall action slots m. { strokeWidth :: Number, strokeColor :: P.Item, handleEvents :: Boolean } -> Position -> H.ComponentHTML action slots m
linkShape p position =
    HS.line
        [ HSA.x1 position.from.x
        , HSA.y1 position.from.y
        , HSA.x2 position.to.x
        , HSA.y2 position.to.y
        , HSA.stroke $ Just $ P.hColorOf p.strokeColor
        , HSA.strokeWidth p.strokeWidth
        , HHP.class_ $ H.ClassName $ if p.handleEvents then "noodle-enable-events" else "noodle-disable-events"
        ]