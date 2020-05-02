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

import Data.Vec2 (Vec2(..))
import Data.Spread as Spread

import Data.Time.Duration (Milliseconds(..))
import Data.DateTime.Instant (unInstant)
import Debug.Trace as DT

import Noodle.UUID as UUID
import Noodle.Process as R
import Noodle.Toolkit as T
import Noodle.Toolkit (withInlets, withOutlets, (~<), (>~))

import Noodle.Render.Atom as R -- FIXME: shouldn't require `Render` module

import Example.Toolkit.Value (RgbaColor(..), Value)
import Example.Toolkit.Value (Value(..), fill, move) as V
import Example.Toolkit.Channel
import Example.Toolkit.OnCanvas (apply) as Canvas


type ProcessF = R.Receive Value -> Effect (R.Send Value)

type ProcessST s = s /\ R.Receive Value -> Effect (s /\ R.Send Value)

type NodeDef = T.NodeDef Value Channel


data Node
    = BangNode
    | NumberNode
    | RandomNode
    | VectorNode
    | ColorNode
    | TimeNode
    | FillNode
    | MoveNode
    -- | SineNode
    | CanvasNode
    | ShapeNode
    | SpreadNode
    | JoinNode
    | NodeListNode



instance showNode :: Show Node where
    show NodeListNode = "node list"
    show BangNode = "bang"
    show RandomNode = "random"
    show NumberNode = "number"
    show VectorNode = "vector"
    show FillNode = "fill"
    show MoveNode = "move"
    show TimeNode = "time"
    -- show SineNode = "sine"
    show ColorNode = "color"
    show ShapeNode = "shape"
    show SpreadNode = "spread"
    show JoinNode = "join"
    show CanvasNode = "canvas"


nodesForTheList :: Array Node
nodesForTheList =
    [ BangNode
    , NumberNode
    , VectorNode
    , RandomNode
    , ColorNode
    , FillNode
    , MoveNode
    , TimeNode
    -- , SineNode
    , ShapeNode
    , SpreadNode
    , JoinNode
    , CanvasNode
    ]


bangNode :: NodeDef
bangNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "bang" /\ TriggerChannel
        , outlets :
            T.withOutlets
            >~ "bang" /\ TriggerChannel
        , process : R.Process pure  -- FIXME: use `PassThrough`
        }


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
            T.withInlets
            ~< "color" /\ ColorChannel
        , outlets :
            T.withOutlets
            >~ "fill" /\ AnyValueChannel
        , process : R.Process
            $ \receive -> let
                send "fill" =
                    case receive "color" of
                        Just (V.Color color) ->
                            Just $ V.fill color
                        Just (V.Spread spread) ->
                            Just $ V.Spread $
                                (\val -> case val of
                                    (V.Color color) -> V.fill color
                                    _ -> val
                                ) <$> spread
                        _ -> Nothing
                send _ =
                    Nothing
            in pure send
        }


moveNode :: NodeDef
moveNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "vec" /\ VectorChannel
        , outlets :
            T.withOutlets
            >~ "move" /\ AnyValueChannel
        , process : R.Process
            $ \receive -> let
                send "move" =
                    case receive "vec" of
                        Just (V.Vector vec) ->
                            Just $ V.move vec
                        Just (V.Spread spread) ->
                            Just $ V.Spread $
                                (\val -> case val of
                                    (V.Vector vec) -> V.move vec
                                    _ -> val
                                ) <$> spread
                        _ -> Nothing
                send _ =
                    Nothing
            in pure send
        }


vectorNode :: NodeDef
vectorNode =
    T.NodeDef
    { inlets :
        withInlets
        ~< "x" /\ NumericalChannel
        ~< "y" /\ NumericalChannel
    , outlets :
        withOutlets
        >~ "vector" /\ AnyValueChannel -- FIXME: Some other channel
    , process : R.Process processF
    }
    where
        processF :: ProcessF
        processF receive = do
            let
                getVector (V.Numerical x) (V.Numerical y) =
                    Just $ V.Vector $ Vec2 x y
                getVector _ _ =
                    Nothing
                send "vector" =
                    getVector
                        <$> receive "x"
                        <*> receive "y"
                        >>= identity
                send _ = Nothing
            pure send


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
        >~ "color" /\ AnyValueChannel
    , process : R.Process processF
    }
    where
        processF :: ProcessF
        processF receive = do
            let
                getColor (V.Numerical r) (V.Numerical g) (V.Numerical b) =
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
                getRandom (V.Numerical min) (V.Numerical max) =
                    randomRange min max
                getRandom _ _ = pure 0.0
            random :: Number <-
                getRandom
                    <$> receive "min"
                    <*> receive "max"
                     #  fromMaybe (pure 0.0)
            let send "random" = Just $ V.Numerical random
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
                spread :: Maybe Value -> Maybe Value -> Maybe Value -> Maybe Value
                spread (Just valueFrom) (Just valueTo) (Just (V.Numerical count)) =
                    Just $ V.Spread $ Spread.make (valueFrom /\ valueTo) (floor count)
                spread (Just valueFrom) Nothing (Just (V.Numerical count)) =
                    Just $ V.Spread $ Spread.repeat (floor count) valueFrom
                spread Nothing (Just valueTo) (Just (V.Numerical count)) =
                    Just $ V.Spread $ Spread.repeat (floor count) valueTo
                spread (Just valueFrom) (Just valueTo) Nothing =
                    Just $ V.Spread $ Spread.make (valueFrom /\ valueTo) 1
                spread (Just valueFrom) Nothing Nothing =
                    Just $ V.Spread $ Spread.singleton valueFrom
                spread Nothing (Just valueTo) Nothing =
                    Just $ V.Spread $ Spread.singleton valueTo
                spread _ _ _ = Nothing
            let send "spread" =
                    spread (receive "from") (receive "to") (receive "count")
                send _ = Nothing
            pure send



joinNode :: NodeDef
joinNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "spread1" /\ AnyValueChannel -- FIXME: Only-spread channel?
            ~< "spread2" /\ AnyValueChannel -- FIXME: Only-spread channel?
        , outlets :
            T.withOutlets
            >~ "join" /\ AnyValueChannel -- FIXME: Some other channel?
        , process : R.Process processF
        }
    where
        processF :: ProcessF
        processF receive = do
            let
                join :: Maybe Value -> Maybe Value -> Maybe Value
                join (Just (V.Spread spreadA)) (Just (V.Spread spreadB)) =
                    Just $ V.Spread (uncurry V.Pair <$> Spread.join spreadA spreadB)
                join (Just (V.Spread spread)) (Just val) =
                    Just $ V.Spread (uncurry V.Pair <$> Spread.join spread (Spread.singleton val))
                join (Just val) (Just (V.Spread spread)) =
                    Just $ V.Spread (uncurry V.Pair <$> Spread.join (Spread.singleton val) spread)
                join (Just (V.Spread spread)) Nothing =
                    Just $ V.Spread spread
                join Nothing (Just (V.Spread spread)) =
                    Just $ V.Spread spread
                join _ _ = Nothing
            let send "join" = join (receive "spread1") (receive "spread2")
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
                posX = (prev.last - prev.start + dt) / 500.0
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
                        -- translate ctx
                        --     { translateX : posX
                        --     , translateY : 20.0
                        --     }
                        setFillStyle ctx "#000000"
                        fillText ctx ("start:" <> show prev.start) 40.0 20.0
                        fillText ctx ("curTime:" <> show curTime) 40.0 40.0
                        fillText ctx ("prev.last:" <> show prev.last) 40.0 60.0
                        fillText ctx ("dt:" <> show dt) 40.0 100.0
                        case receive "scene" of
                            Just value ->
                                Canvas.apply value ctx
                            _ -> pure unit
                        fillPath ctx $ do
                            moveTo ctx 10.0 10.0
                            lineTo ctx 20.0 20.0
                            lineTo ctx 10.0 20.0
                            closePath ctx
                    -- pure unit
                Nothing ->
                    pure unit
            let send "x" = Just $ V.Numerical 1.0
                send _ = Nothing
            pure $ next { ctx = maybeContext } /\ send
            where
                getContext :: Effect (Maybe Context2D)
                getContext = do
                    maybeCanvas <- getCanvasElementById "the-canvas"
                    case maybeCanvas of
                        Just canvas -> do
                            ctx <- getContext2D canvas
                            pure $ pure ctx
                        Nothing -> pure Nothing
                    {-
                    case prev.ctx of
                        Just ctx -> pure $ pure ctx
                        Nothing ->
                            runMaybeT $ do
                                canvas <- MaybeT $ getCanvasElementById "the-canvas"
                                lift $ getContext2D canvas
                    -}


instance nodeAtom :: R.Atom Node where
    labelOf = show
    uniqueIdOf = show
    debugInfoOf = show
