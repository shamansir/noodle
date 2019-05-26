module Example.Toolkit
    ( Value(..)
    , Channel(..)
    , ParticleShape(..)
    , toolkit
    ) where

import Prelude

import Effect (Effect)
import Effect.Random (randomRange)

import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Lens ((^.))
import Data.Lens.At (at)
import Data.List ((:))
import Data.List as List
import Data.List.Lazy (List(..))
import Data.Map as Map
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))

import Rpd.Process (ProcessF(..)) as R
import Rpd.Toolkit as Rpd
import Rpd.Util (type (/->))


data ParticleShape
    = Circle
    | Cross
    | Square
    | Diamond


data Value
    = Bang
    | Color Number Number Number
    | Shape ParticleShape
    | Random Number
    | Number' Number
    | Trigger Boolean
    | Period Number
    | Magic Number Number


data Channel
    = ColorChannel
    | ShapeChannel
    | NumberChannel
    | TimeChannel
    | TriggerChannel


instance showValue :: Show Value where
    show _ = "DATA"
    -- show Bang = "bang"
    -- show (Color r g b) = "color"
    -- show (Shape shape) = "shape"
    -- show (Random n) = "random"
    -- show (Number' n) = "number"


instance showChannel :: Show Channel where
    show ColorChannel = "color"
    show ShapeChannel = "shape"
    show NumberChannel = "number"
    show TimeChannel = "time"
    show TriggerChannel = "trigger"


instance exampleChannel :: Rpd.Channel Channel Value where
    default _ = Bang
    accept _ _ = true
    adapt _ = identity


toolkit :: Rpd.Toolkit Channel Value
toolkit =
    { name : Rpd.ToolkitName "example"
    , nodes
    }
    where
        nodes :: Rpd.NodeDefAlias /-> Rpd.NodeDef Channel Value
        nodes =
            [ "random" /\ randomNode
            ]
            # map (bimap Rpd.NodeDefAlias identity)
            # Map.fromFoldable


randomNode :: Rpd.NodeDef Channel Value
randomNode =
    { inlets :
        [ "bang" /\ TriggerChannel
        , "min" /\ NumberChannel
        , "max" /\ NumberChannel
        ]
        # map (bimap Rpd.InletAlias identity)
        # List.fromFoldable
    , outlets :
        [ "random" /\ NumberChannel ]
        # map (bimap Rpd.OutletAlias identity)
        # List.fromFoldable
    , process : R.Process processF
    }
    where
        processF :: (String -> Maybe Value) -> Effect (String -> Maybe Value)
        processF receive = do
            let
                min = receive "min" # fromMaybe (Number' 0.0)
                max = receive "max" # fromMaybe (Number' 0.0)
            random <-
                case min /\ max of
                    (Number' min' /\ Number' max') ->
                        randomRange min' max'
                    _ -> pure 0.0
            let send "random" = Just $ Number' random
                send _ = Nothing
            pure send


-- patch :: Rpd.PatchDef Value
-- patch =
--     { name : "particles"
--     , nodeDefs
--         : colorNode
--         : metroNode
--         : List.Nil
--     }


-- numberInlet :: String -> Rpd.InletDef Value
-- numberInlet label =
--     { label : label
--     , default : Just (Number' 0.0)
--     , accept : Just acceptF
--     }
--     where
--         acceptF (Number' _) = true
--         acceptF _ = false



-- colorInlet :: String -> Rpd.InletDef Value
-- colorInlet label =
--     { label : label
--     , default : Just (Color 0.0 0.0 0.0)
--     , accept : Just acceptF
--     }
--     where
--         acceptF (Color _ _ _) = true
--         acceptF _ = false


-- colorOutlet :: String -> Rpd.OutletDef Value
-- colorOutlet label =
--     { label : label
--     , accept : Just acceptF
--     }
--     where
--         acceptF (Color _ _ _) = true
--         acceptF _ = false


-- bangOutlet :: String -> Rpd.OutletDef Value
-- bangOutlet label =
--     { label : label
--     , accept : Just acceptF
--     }
--     where
--         acceptF Bang = true
--         acceptF _ = false


-- periodInlet :: String -> Rpd.InletDef Value
-- periodInlet label =
--     { label : label
--     , default : Just (Period 0.0)
--     , accept : Just acceptF
--     }
--     where
--         acceptF (Period _) = true
--         acceptF _ = false


-- triggerInlet :: String -> Rpd.InletDef Value
-- triggerInlet label =
--     { label : label
--     , default : Just $ Trigger false
--     , accept : Just acceptF
--     }
--     where
--         acceptF (Trigger _) = true
--         acceptF _ = false



-- colorNode :: Rpd.NodeDef Value
-- colorNode =
--     { name : "color"
--     , inletDefs
--         : numberInlet "r"
--         : numberInlet "g"
--         : numberInlet "b"
--         : List.Nil
--     , outletDefs :
--         List.singleton $ colorOutlet "color"
--     , process : R.FoldedByLabel foldToColor
--     }
--     where
--         foldToColor (R.InletsMapData m) =
--             R.OutletsMapData
--                 $ fromMaybe Map.empty
--                 $ buildColor <$> (m^.at "r") <*> (m^.at "g") <*> (m^.at "b")
--         buildColor (Number' r) (Number' g) (Number' b) =
--             Map.empty # Map.insert "color" (Color r g b)
--         buildColor _ _ _ =
--             Map.empty



-- metroNode :: Rpd.NodeDef Value
-- metroNode =
--     { name : "metro"
--     , inletDefs
--         : triggerInlet "enabled"
--         : periodInlet "period"
--         : List.Nil
--     , outletDefs :
--         List.singleton $ bangOutlet "bang"
--     , process : R.FoldedByLabel foldToBang
--     }
--     where
--         foldToBang (R.InletsMapData m) =
--             R.OutletsMapData
--                 $ fromMaybe Map.empty
--                 $ sendBang <$> (m^.at "enabled") <*> (m^.at "period")
--         sendBang (Trigger isEnabled) (Period period) =
--             if isEnabled then
--                 Map.empty # Map.insert "bang" Bang
--             else Map.empty
--         sendBang _ _ =
--             Map.empty



-- -- TODO: metro, color, random, shape, magic, wind...


-- instance isDataValue :: Rpd.IsData Value where
--   default = Period 5.0
