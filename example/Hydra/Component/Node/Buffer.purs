module Hydra.Component.Node.Buffer where


import Prelude

import Effect.Class (class MonadEffect)
import Color as C
import Math (floor)

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (fromMaybe)
import Data.Array ((:))
import Data.Array as Array
import Data.Map as Map
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec ((!!))
import Data.Typelevel.Num.Reps (d0, d1, d2)

import App.Toolkit.UI (FromNode(..)) as UI
import App.Emitters as E

import JetBrains.Palettes as P

import Noodle.Node (Node)
import Noodle.Node as Node

import Hydra (Hydra, Buffer(..), Texture(..), Value(..))
import Hydra as Hydra
import Hydra.Queue (Queue)
import Hydra.Queue as Queue
import Hydra.Extract as HydraE
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
    { mode :: Mode, buffer :: Buffer, queue :: Queue }


data Mode
    = ToBuffer
    | FromBuffer


data Action
    = Initialize
    | Receive Queue
    | Select Buffer
    | Store Texture



outputBuffers :: Array Buffer
outputBuffers = [ O0, O1, O2, O3 ]


sourceBuffers :: Array Buffer
sourceBuffers = [ S0, S1, S2, S3 ]


bodyWidth = 110.0 -- FIXME: pass from outside


-- defaultPalette :: Hydra.Color
-- defaultPalette = Hydra.Color { r = Num


initialState :: Mode -> UI.NodeInput -> State
initialState mode { patchState } =
    { mode, buffer : O0, queue : patchState }


render :: forall m. State -> H.ComponentHTML Action () m
render { mode, buffer, queue } =
    HS.g
        [ HSA.class_ $ H.ClassName "buffer-node" ]
        [ (HS.g [] $ Array.mapWithIndex (bufferButton  0.0 $ C.rgb 0 0 127) sourceBuffers)
        , (HS.g [] $ Array.mapWithIndex (bufferButton 18.0 $ C.rgb 0 127 0) outputBuffers)
        ]
    where
        bufferButton y color idx buffer =
            HS.g
                [ HSA.class_ $ H.ClassName "buffer-button" ]
                [ HS.circle
                    [ HSA.cx $ 7.0 + (toNumber $ idx `mod` 9) * 12.0
                    , HSA.cy $ y + 7.0 + (floor $ toNumber idx / 9.0) * 12.0
                    , HSA.stroke $ Just $ C.toSvg color
                    , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
                    , HSA.strokeWidth 1.0
                    , HSA.r 5.0
                    ]
                , HS.rect
                    [ HSA.x $ 2.0 + (toNumber $ idx `mod` 9) * 12.0
                    , HSA.y $ y + 2.0 + (floor $ toNumber idx / 9.0) * 12.0
                    , HSA.width 10.0
                    , HSA.height 10.0
                    , HSA.stroke $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
                    , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
                    , HE.onClick $ const $ Select buffer
                    -- , HP.style "cursor: pointer"
                    ]
                ]


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () UI.NodeOutput m Unit
handleAction = case _ of
    Initialize -> do
        { mode } <- H.get
        case mode of
            ToBuffer -> pure unit -- TODO: subscribe inlet
            FromBuffer -> pure unit -- TODO: do nothing
    Receive queue ->
        H.modify_ (_ { queue = queue })
    Select buffer -> do
        { mode, queue } <- H.modify (_ { buffer = buffer })
        case mode of
            ToBuffer -> pure unit -- TODO: do nothing
            FromBuffer ->
                case Queue.textureAt buffer queue of
                    Just texture -> H.raise $ UI.SendToOutlet "texture" $ Hydra.hydraOf texture
                    Nothing -> pure unit
        {- H.raise $ UI.SendToInlet "out" $ case curMode of
            In -> Hydra.justModifier $ Hydra.color $ P.toColorMod palette
            Out -> Hydra.hydraOf $ Hydra.textureOf $ P.toSolidSource palette -}
    Store texture -> do
            { mode, buffer } <- H.get
            case mode of
                ToBuffer ->
                    H.raise $ UI.ToPatch $ Queue.Store buffer texture
                FromBuffer -> pure unit -- do nothing


component :: forall m. Mode -> MonadEffect m => UI.NodeComponent m
component mode =
    H.mkComponent
        { initialState : initialState mode
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive <<< _.patchState
            }
        }