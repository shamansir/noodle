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
import Data.Maybe (Maybe(..), maybe)
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
    { mode :: Mode
    , buffer :: Buffer
    , queue :: Queue
    , node :: Node Hydra
    }


data Mode
    = ToBuffer
    | FromBuffer


data Action
    = NoOp
    | Initialize
    | Receive Queue
    | Select Buffer
    | Store Texture



outputBuffers :: Array Buffer
outputBuffers = [ O0, O1, O2, O3 ]


sourceBuffers :: Array Buffer
sourceBuffers = [ S0, S1, S2, S3 ]


bodyWidth = 110.0 -- FIXME: pass from outside
bodyHeight = 80.0 -- FIXME: pass from outside


-- defaultPalette :: Hydra.Color
-- defaultPalette = Hydra.Color { r = Num


initialState :: Mode -> UI.NodeInput -> State
initialState mode { patchState, node } =
    { mode, buffer : O0, queue : patchState, node }


render :: forall m. State -> H.ComponentHTML Action () m
render { mode, buffer, queue } =
    HS.g
        [ HSA.class_ $ H.ClassName "buffer-node" ]
        [ (HS.g [] $ Array.mapWithIndex (bufferButton  0.0 $ C.rgb 200 200 200) sourceBuffers)
        , (HS.g [] $ Array.mapWithIndex (bufferButton 18.0 $ C.rgb 127 127 127) outputBuffers)
        ]
    where
        fillFor otherBuffer =
            if otherBuffer == buffer
                then C.rgba 255 255 255 $ if isOwned otherBuffer then 0.5 else 1.0
                else if isOwned otherBuffer then C.rgba 100 100 100 1.0 else C.rgba 0 0 0 0.0
        isOwned =
            flip Queue.isOwned queue
        circleRadius = 5.0
        circleDiameter = circleRadius * 2.0
        circleDiameterAndPadding = circleDiameter + 2.0
        xFor idx = (bodyWidth / 2.0) - (4.0 * circleDiameter / 2.0) + (toNumber $ idx `mod` 9) * circleDiameterAndPadding
        yFor _   = (bodyHeight / 2.0) - (circleDiameter * 2.0)
        bufferButton y color idx otherBuffer =
            HS.g
                [ {-HSA.class_ $ H.ClassName "buffer-button"
                , -} HSA.class_ $ H.ClassName $ if isOwned otherBuffer then "buffer-button-owned" else "buffer-button-free"
                ]
                [ HS.circle
                    [ HSA.cx $ xFor idx
                    , HSA.cy $ y + yFor idx
                    , HSA.stroke $ Just $ C.toSvg color
                    , HSA.fill $ Just $ C.toSvg $ fillFor otherBuffer
                    , HSA.strokeWidth 1.0
                    , HSA.r circleRadius
                    ]
                , HS.rect
                    [ HSA.x $ xFor idx - circleRadius
                    , HSA.y $ y + yFor idx - circleRadius
                    , HSA.width circleDiameter
                    , HSA.height circleDiameter
                    , HSA.stroke $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
                    , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
                    , HE.onClick $ const $ if isOwned otherBuffer then NoOp else Select otherBuffer
                    -- , HP.style "cursor: pointer"
                    ]
                ]


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () UI.NodeOutput m Unit
handleAction = case _ of
    NoOp -> pure unit
    Initialize -> do
        { mode, node } <- H.get
        case mode of
            ToBuffer -> do
                emitter <- E.fromInlet node "texture"
                _ <- H.subscribe (maybe NoOp Store <<< Hydra.toTexture <$> emitter)
                pure unit
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