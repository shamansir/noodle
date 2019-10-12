module Example.Toolkit.Nodes where

import Prelude

import Effect (Effect)
import Effect.Random (randomRange)

import Data.Maybe
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)

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
    show CanvasNode = "time"
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
            ~< "min"  /\ NumericChannel
            ~< "max"  /\ NumericChannel
        , outlets :
            withOutlets
            >~ "random" /\ NumericChannel
        , process : R.Process processF
        }
    where
        processF :: (String -> Maybe Value) -> Effect (String -> Maybe Value)
        processF receive = do
            let
                getRandom (Numeric min) (Numeric max) =
                    randomRange min max
                getRandom _ _ =
                    randomRange 0.0 100.0
            random <- (getRandom <$> receive "min" <*> receive "max")
                            # fromMaybe (pure 0.0)
            let send "random" = Just $ Numeric random
                send _ = Nothing
            pure send


sineNode :: T.NodeDef Value Channel
sineNode =
    T.NodeDef
        { inlets :
            withInlets
            ~< "bang" /\ TriggerChannel
            ~< "min"  /\ NumericChannel
            ~< "max"  /\ NumericChannel
        , outlets :
            withOutlets
            >~ "random" /\ NumericChannel
        , process : R.Process processF
        }
    where
        processF :: (String -> Maybe Value) -> Effect (String -> Maybe Value)
        processF receive = do
            let
                min = receive "min" # fromMaybe (Numeric 0.0)
                max = receive "max" # fromMaybe (Numeric 100.0)
            random <-
                case min /\ max of
                    (Numeric min' /\ Numeric max') ->
                        randomRange min' max'
                    _ -> pure 0.0
            let send "random" = Just $ Numeric random
                send _ = Nothing
            pure send


timeNode :: T.NodeDef Value Channel
timeNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "time" /\ NumericChannel
        , outlets :
            T.withOutlets
            >~ "time" /\ NumericChannel
        , process : R.Process pure  -- FIXME: use `PassThrough`
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
            >~ "x" /\ NumericChannel
        , process : R.Process processF
        }
    where
        -- animationInlet :: AnimPart -> String /\ Channel
        -- animationInlet animPart =
        --     show animPart /\ AnimationChannel
        processF :: (String -> Maybe Value) -> Effect (String -> Maybe Value)
        processF receive = do
            let (maybeTime :: Maybe Value) = receive "time"
            -- _ <- drawScene
            --     $ Animation.export
            --     $ Animation.Scene
            --         { initial : Animation.assets
            --         , transforms : unit
            --         , time : case maybeTime of
            --             Just (Time t) -> Just t
            --             _ -> instant $ Milliseconds 0.0
            --         }
            let send "x" = Just $ Numeric 1.0
                send _ = Nothing
            pure send
