module Example.Toolkit.Nodes where

import Prelude

import Effect (Effect)
import Effect.Random (randomRange)
import Effect.Now (now)

import Graphics.Canvas

import Data.Maybe
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)

import Data.Time.Duration (Milliseconds(..))
import Data.DateTime.Instant (unInstant)

import Rpd.Process as R
import Rpd.Toolkit as T
import Rpd.Toolkit (withInlets, withOutlets, (~<), (>~))

import Example.Toolkit.Value
import Example.Toolkit.Channel


data Node
    = RandomNode
    | NodeListNode
    | TimeNode
    | SineNode
    | CanvasNode
    | ButtonsNode


instance showNode :: Show Node where
    show RandomNode = "random"
    show NodeListNode = "node list"
    show TimeNode = "time"
    show SineNode = "sine"
    show CanvasNode = "canvas"
    show ButtonsNode = "buttons"


nodesForTheList :: Array Node
nodesForTheList =
    [ RandomNode, CanvasNode, SineNode, TimeNode ]


randomNode :: T.NodeDef Value Channel
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
        processF :: (String -> Maybe Value) -> Effect (String -> Maybe Value)
        processF receive = do
            let
                getRandom (Numerical min) (Numerical max) =
                    randomRange min max
                getRandom _ _ =
                    randomRange 0.0 100.0
            random <- (getRandom <$> receive "min" <*> receive "max")
                            # fromMaybe (pure 0.0)
            let send "random" = Just $ Numerical random
                send _ = Nothing
            pure send


sineNode :: T.NodeDef Value Channel
sineNode =
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
        processF :: (String -> Maybe Value) -> Effect (String -> Maybe Value)
        processF receive = do
            let
                min = receive "min" # fromMaybe (Numerical 0.0)
                max = receive "max" # fromMaybe (Numerical 100.0)
            random <-
                case min /\ max of
                    (Numerical min' /\ Numerical max') ->
                        randomRange min' max'
                    _ -> pure 0.0
            let send "random" = Just $ Numerical random
                send _ = Nothing
            pure send


timeNode :: T.NodeDef Value Channel
timeNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "time" /\ NumericalChannel
        , outlets :
            T.withOutlets
            >~ "time" /\ NumericalChannel
        , process : R.Process pure  -- FIXME: use `PassThrough`
        }


type CanvasState =
    { start :: Number
    , last :: Number
    }


initialCanvasState :: CanvasState
initialCanvasState =
    { start : -1.0
    , last : 0.0
    }


canvasNode :: T.NodeDef Value Channel
canvasNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "time" /\ TimeChannel
            -- ~<< (fromFoldable $ animationInlet <$> allManParts)
        , outlets :
            -- T.noOutlets
            T.withOutlets
            >~ "x" /\ NumericalChannel
        , process : R.ProcessST $ R.makeProcessST initialCanvasState processF
        }
    where
        processF :: CanvasState /\ R.Receive Value -> Effect (CanvasState /\ R.Send Value)
        processF (prev /\ receive) = do
            (Milliseconds curTime) <- unInstant <$> now
            let
                (maybeTime :: Maybe Value) = receive "time"
                dt = curTime - prev.last
                posX = (prev.last - prev.start + dt) / 1000.0
                next =
                    if prev.start < 0.0 then
                        { start : curTime
                        , last : curTime
                        }
                    else
                        prev
                            { last = curTime
                            }
            maybeCanvas :: Maybe CanvasElement <- getCanvasElementById "the-canvas"
            case maybeCanvas of
                Just canvas -> do
                    -- TODO: requestAnimationFrame $ do
                    ctx <- getContext2D canvas
                    withContext ctx $ do
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
                    pure unit
                Nothing -> pure unit
            let send "x" = Just $ Numerical 1.0
                send _ = Nothing
            pure $ next /\ send
