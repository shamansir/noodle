module Example.Toolkit.Nodes where

import Prelude

import Math (pi) as Math

import Effect (Effect)
import Effect.Random (randomRange)
import Effect.Now (now)
import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..), lift)

import Graphics.Canvas

import Data.Int (round, floor)
import Data.Maybe
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_, for_)
import Data.Array (catMaybes) as Array

import Data.Spread as Spread

import Data.Time.Duration (Milliseconds(..))
import Data.DateTime.Instant (unInstant)
import Debug.Trace as DT

import Rpd.UUID as UUID
import Rpd.Process as R
import Rpd.Toolkit as T
import Rpd.Toolkit (withInlets, withOutlets, (~<), (>~))

import Example.Toolkit.Value
import Example.Toolkit.Value (Value(..)) as V
import Example.Toolkit.Channel
import Example.Toolkit.OnCanvas (apply) as Canvas


type ProcessF = R.Receive Value -> Effect (R.Send Value)

type ProcessST s = s /\ R.Receive Value -> Effect (s /\ R.Send Value)

type NodeDef = T.NodeDef Value Channel


data Node
    = NumberNode
    | RandomNode
    | ColorNode
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
    show ColorNode = "color"
    show ShapeNode = "shape"
    show SpreadNode = "spread"
    show PairNode = "pair"
    show CanvasNode = "canvas"


nodesForTheList :: Array Node
nodesForTheList =
    [ NumberNode
    , RandomNode
    , ColorNode
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


colorNode :: NodeDef
colorNode =
    T.NodeDef
    { inlets :
        withInlets
        ~< "r" /\ NumericalChannel
        ~< "g" /\ NumericalChannel
        ~< "b" /\ NumericalChannel
    , outlets :
        withOutlets
        >~ "color" /\ AnyValueChannel -- FIXME: Some other channel
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
            ~< "shape" /\ AnyValueChannel -- FIXME: Some other channel
        , outlets :
            T.withOutlets
            >~ "shape" /\ AnyValueChannel -- FIXME: Some other channel
        , process : R.Process pure  -- FIXME: use `PassThrough`
        }


spreadNode :: NodeDef
spreadNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "from" /\ AnyValueChannel
            ~< "to" /\ AnyValueChannel
            ~< "count" /\ NumericalChannel
        , outlets :
            T.withOutlets
            >~ "spread" /\ SpreadChannel -- FIXME: Only-spread channel?
        , process : R.Process processF
        }
    where
        processF :: ProcessF
        processF receive = do
            let
                spread :: Value -> Value -> Value -> Maybe Value
                spread valueFrom valueTo (Numerical count) =
                    Just $ Spread (Spread.make (valueFrom /\ valueTo) (floor count))
                spread _ _ _ = Nothing
            let send "spread" =
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
            ~< "spread1" /\ AnyValueChannel -- FIXME: Only-spread channel?
            ~< "spread2" /\ AnyValueChannel -- FIXME: Only-spread channel?
        , outlets :
            T.withOutlets
            >~ "pair" /\ AnyValueChannel -- FIXME: Some other channel?
        , process : R.Process processF
        }
    where
        processF :: ProcessF
        processF receive = do
            let
                pair :: Value -> Value -> Maybe Value
                pair (Spread spreadA) (Spread spreadB) =
                    Just $ Spread (uncurry Pair <$> Spread.join spreadA spreadB)
                pair (Spread spread) val =
                    Just $ Spread (uncurry Pair <$> Spread.join spread (Spread.singleton val))
                pair val (Spread spread) =
                    Just $ Spread (uncurry Pair <$> Spread.join (Spread.singleton val) spread)
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


canvasNode :: NodeDef
canvasNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "frame" /\ TriggerChannel
            ~< "scene" /\ AnyValueChannel
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
                        clearRect ctx { x : 0.0, y : 0.0, width : 500.0, height : 500.0 }
                        fillText ctx ("start:" <> show prev.start) 40.0 20.0
                        fillText ctx ("curTime:" <> show curTime) 40.0 40.0
                        fillText ctx ("prev.last:" <> show prev.last) 40.0 60.0
                        fillText ctx ("dt:" <> show dt) 40.0 100.0
                        setFillStyle ctx "#000000"
                        case receive "scene" of
                            Just (Spread spread) ->
                                for_ (Array.catMaybes $ Spread.run spread) $ flip Canvas.apply ctx
                                --flip apply ctx <*> instructions
                            Just value ->
                                Canvas.apply value ctx
                            _ -> pure unit
                        translate ctx
                            { translateX : posX
                            , translateY : 20.0
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
