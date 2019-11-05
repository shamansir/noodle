module Example.Toolkit.Nodes where

import Prelude

import Effect (Effect)
import Effect.Random (randomRange)
import Effect.Now (now)
import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..), lift)

import Graphics.Canvas

import Data.Int (round)
import Data.Maybe
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)

import Data.Time.Duration (Milliseconds(..))
import Data.DateTime.Instant (unInstant)

import Rpd.UUID as UUID
import Rpd.Process as R
import Rpd.Toolkit as T
import Rpd.Toolkit (withInlets, withOutlets, (~<), (>~))

import Example.Toolkit.Value
import Example.Toolkit.Value (Value(..)) as V
import Example.Toolkit.Channel


type ProcessF = R.Receive Value -> Effect (R.Send Value)

type ProcessST s = s /\ R.Receive Value -> Effect (s /\ R.Send Value)

type NodeDef = T.NodeDef Value Channel


data Node
    = NumberNode
    | RandomNode
    | FillNode
    | TimeNode
    -- | SineNode
    | CanvasNode
    | ShapeNode
    | SpreadNode
    | PairNode
    | NodeListNode



instance showNode :: Show Node where
    show NodeListNode = "node list"
    show RandomNode = "random"
    show NumberNode = "number"
    show TimeNode = "time"
    -- show SineNode = "sine"
    show FillNode = "fill"
    show ShapeNode = "shape"
    show SpreadNode = "spread"
    show PairNode = "pair"
    show CanvasNode = "canvas"


nodesForTheList :: Array Node
nodesForTheList =
    [ NumberNode
    , RandomNode
    , FillNode
    , TimeNode
    -- , SineNode
    , ShapeNode
    , SpreadNode
    , PairNode
    , CanvasNode
    ]


numberNode :: NodeDef
numberNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "num" /\ NumericalChannel
        , outlets :
            T.withOutlets
            >~ "num" /\ NumericalChannel
        , process : R.Process pure  -- FIXME: use `PassThrough`
        }


fillNode :: NodeDef
fillNode =
    T.NodeDef
    { inlets :
        withInlets
        ~< "r" /\ NumericalChannel
        ~< "g" /\ NumericalChannel
        ~< "b" /\ NumericalChannel
    , outlets :
        withOutlets
        >~ "fill" /\ InstructionsChannel -- FIXME: Some other channel
    , process : R.Process processF
    }
    where
        processF :: ProcessF
        processF receive = do
            let
                getColor (Numerical r) (Numerical g) (Numerical b) =
                    Just $ V.Color $ RgbaColor { r : r, g : g, b : b, a : 1.0 }
                getColor _ _ _ =
                    Nothing
                send "color" =
                    getColor
                        <$> receive "r"
                        <*> receive "g"
                        <*> receive "b"
                        >>= identity
                send _ = Nothing
            pure send


randomNode :: NodeDef
randomNode =
    T.NodeDef
        { inlets :
            withInlets
            ~< "bang" /\ TriggerChannel
            ~< "min"  /\ NumericalChannel
            ~< "max"  /\ NumericalChannel
        , outlets :
            withOutlets
            >~ "random" /\ NumericalChannel
        , process : R.Process processF
        }
    where
        processF :: ProcessF
        processF receive = do
            let
                getRandom :: Value -> Value -> Effect Number
                getRandom (Numerical min) (Numerical max) =
                    randomRange min max
                getRandom _ _ = pure 0.0
            random :: Number <-
                getRandom
                    <$> receive "min"
                    <*> receive "max"
                     #  fromMaybe (pure 0.0)
            let send "random" = Just $ Numerical random
                send _ = Nothing
            pure send



timeNode :: NodeDef
timeNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "time" /\ TriggerChannel
        , outlets :
            T.withOutlets
            >~ "time" /\ TriggerChannel
        , process : R.Process pure  -- FIXME: use `PassThrough`
        }


shapeNode :: NodeDef
shapeNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "shape" /\ InstructionsChannel -- FIXME: Some other channel
        , outlets :
            T.withOutlets
            >~ "shape" /\ InstructionsChannel -- FIXME: Some other channel
        , process : R.Process pure  -- FIXME: use `PassThrough`
        }


spreadNode :: NodeDef
spreadNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "from" /\ InstructionsChannel -- FIXME: Some other channel
            ~< "to" /\ InstructionsChannel -- FIXME: Some other channel
            ~< "count" /\ NumericalChannel
        , outlets :
            T.withOutlets
            >~ "spread" /\ InstructionsChannel -- FIXME: Only-spread channel?
        , process : R.Process processF
        }
    where
        processF :: ProcessF
        processF receive = do
            let
                spread :: Value -> Value -> Value -> Maybe Value
                spread (Instructions from) (Instructions to) (Numerical count) =
                    Just $ Instructions $ Spread $ Interpolation
                        { from : from, to : to, count : round count }
                spread _ _ _ = Nothing
            let send "pair" =
                    spread
                    <$> receive "from"
                    <*> receive "to"
                    <*> receive "count"
                    >>= identity
                send _ = Nothing
            pure send


pairNode :: NodeDef
pairNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "spread1" /\ InstructionsChannel -- FIXME: Only-spread channel?
            ~< "spread2" /\ InstructionsChannel -- FIXME: Only-spread channel?
        , outlets :
            T.withOutlets
            >~ "pair" /\ InstructionsChannel -- FIXME: Some other channel?
        , process : R.Process processF
        }
    where
        processF :: ProcessF
        processF receive = do
            let
                pair :: Value -> Value -> Maybe Value
                pair (Instructions a) (Instructions b) =
                    Just $ Instructions $ Pair a b
                pair _ _ = Nothing
            let send "pair" =
                    pair
                    <$> receive "spread1"
                    <*> receive "spread2"
                    >>= identity
                send _ = Nothing
            pure send


type CanvasState =
    { ctx :: Maybe Context2D
    , start :: Number
    , last :: Number
    }


initialCanvasState :: CanvasState
initialCanvasState =
    { ctx : Nothing
    , start : -1.0
    , last : 0.0
    }


class OnCanvas x where
    apply :: x -> Context2D -> Effect Unit


instance instructionOnCanvas :: OnCanvas Instruction where
    apply _ _ = do
        let i = lerp (NoOp /\ NoOp) 1.0
        pure unit


canvasNode :: NodeDef
canvasNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "frame" /\ TriggerChannel
            ~< "scene" /\ InstructionsChannel
        , outlets :
            T.noOutlets
            -- T.withOutlets
            -- >~ "x" /\ NumericalChannel
        , process : R.ProcessST $ R.makeProcessST initialCanvasState processF
        }
    where
        processF :: ProcessST CanvasState
        processF (prev /\ receive) = do
            (Milliseconds curTime) <- unInstant <$> now
            let
                -- (maybeTime :: Maybe Value) = receive "frame"
                dt = curTime - prev.last
                posX = (prev.last - prev.start + dt) / 1000.0
                next =
                    if prev.start < 0.0 then
                        prev
                            { start = curTime
                            , last = curTime
                            }
                    else
                        prev
                            { last = curTime
                            }
            maybeContext :: Maybe Context2D <- getContext
            _ <- case maybeContext of
                Just ctx ->
                    withContext ctx $ do
                        case receive "scene" of
                            Just (Instructions instruction) ->
                                apply instruction ctx
                            _ -> pure unit
                        clearRect ctx { x : 0.0, y : 0.0, width : 500.0, height : 500.0 }
                        fillText ctx ("start:" <> show prev.start) 40.0 20.0
                        fillText ctx ("curTime:" <> show curTime) 40.0 40.0
                        fillText ctx ("prev.last:" <> show prev.last) 40.0 60.0
                        fillText ctx ("dt:" <> show dt) 40.0 100.0
                        translate ctx
                            { translateX : posX
                            , translateY : 0.0
                            }
                        setFillStyle ctx "#0000FF"
                        fillPath ctx $ do
                            moveTo ctx 10.0 10.0
                            lineTo ctx 20.0 20.0
                            lineTo ctx 10.0 20.0
                            closePath ctx
                    -- pure unit
                Nothing ->
                    pure unit
            let send "x" = Just $ Numerical 1.0
                send _ = Nothing
            pure $ next { ctx = maybeContext } /\ send
            where
                getContext :: Effect (Maybe Context2D)
                getContext = case prev.ctx of
                    Just ctx -> pure $ pure ctx
                    Nothing ->
                        runMaybeT $ do
                            canvas <- MaybeT $ getCanvasElementById "the-canvas"
                            lift $ getContext2D canvas
