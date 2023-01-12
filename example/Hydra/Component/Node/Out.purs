module Hydra.Component.Node.Out where


import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Color as C
import Math (floor)

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (fromMaybe)
import Data.Array ((:))
import Data.Array as Array
import Data.Map as Map
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec ((!!))
import Data.Typelevel.Num.Reps (d0, d1, d2)

import Web.App.Toolkit.UI (FromNode(..)) as UI
import Web.Emitters  as E

import JetBrains.Palettes as P

import Noodle.Node (Node)
import Noodle.Node as Node

import Hydra (Hydra, Buffer(..), Texture(..), Value(..))
import Hydra as Hydra
import Hydra.Queue (Queue)
import Hydra.Queue as Queue
import Hydra.Extract as HydraE
import Hydra.Compile as Hydra
import Hydra.Compile (friendly, compact) as Compiler
import Hydra.Engine as HydraE
import Hydra.Component.Input as Input
import Hydra.Toolkit.UI.Components (NodeComponent, NodeInput, NodeOutput) as UI
import Hydra.Toolkit.UI.Action as Queue

import Halogen as H
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS
import Halogen.Svg.Attributes as HSA

import Color.Extra (toSvg) as C


type State =
    { node :: Node Hydra
    }


data Action
    = NoOp
    | Initialize
    | Render Texture


bodyWidth = 110.0 -- FIXME: pass from outside


-- defaultPalette :: Hydra.Color
-- defaultPalette = Hydra.Color { r = Num


initialState :: UI.NodeInput -> State
initialState { patchState, node } =
    { node }


render :: forall m. State -> H.ComponentHTML Action () m
render = const $
    HS.g
        [ ]
        [ ]


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () UI.NodeOutput m Unit
handleAction = case _ of
    NoOp -> pure unit
    Initialize -> do
        { node } <- H.get
        emitter <- E.fromInlet node "texture"
        _ <- H.subscribe (maybe NoOp Render <<< Hydra.toTexture <$> emitter)
        pure unit
    Render texture -> do
        -- { queue } <- H.get
        let queue = Queue.just texture
        liftEffect $ do
            Console.log $ Hydra.compile Compiler.friendly queue
            HydraE.evaluate $ Hydra.compile Compiler.compact queue
        pure unit


component :: forall m. MonadEffect m => UI.NodeComponent m
component =
    H.mkComponent
        { initialState : initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }