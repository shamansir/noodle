module Example.Toolkit
    ( patch
    , colorNode
    , Value(..)
    , ParticleShape(..)
    ) where


import Prelude

import Data.Maybe
import Data.List as List
import Data.List ((:))
import Data.Map as Map
import Data.Lens ((^.))
import Data.Lens.At (at)

import Rpd.Def as Rpd
import Rpd.Process (ProcessF(..), InletsMapData(..), OutletsMapData(..)) as R


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


patch :: Rpd.PatchDef Value
patch =
    { name : "particles"
    , nodeDefs
        : colorNode
        : metroNode
        : List.Nil
    }


numberInlet :: String -> Rpd.InletDef Value
numberInlet label =
    { label : label
    , default : Just (Number' 0.0)
    , accept : Just acceptF
    }
    where
        acceptF (Number' _) = true
        acceptF _ = false



colorInlet :: String -> Rpd.InletDef Value
colorInlet label =
    { label : label
    , default : Just (Color 0.0 0.0 0.0)
    , accept : Just acceptF
    }
    where
        acceptF (Color _ _ _) = true
        acceptF _ = false


colorOutlet :: String -> Rpd.OutletDef Value
colorOutlet label =
    { label : label
    , accept : Just acceptF
    }
    where
        acceptF (Color _ _ _) = true
        acceptF _ = false


bangOutlet :: String -> Rpd.OutletDef Value
bangOutlet label =
    { label : label
    , accept : Just acceptF
    }
    where
        acceptF Bang = true
        acceptF _ = false


periodInlet :: String -> Rpd.InletDef Value
periodInlet label =
    { label : label
    , default : Just (Period 0.0)
    , accept : Just acceptF
    }
    where
        acceptF (Period _) = true
        acceptF _ = false


triggerInlet :: String -> Rpd.InletDef Value
triggerInlet label =
    { label : label
    , default : Just $ Trigger false
    , accept : Just acceptF
    }
    where
        acceptF (Trigger _) = true
        acceptF _ = false



colorNode :: Rpd.NodeDef Value
colorNode =
    { name : "color"
    , inletDefs
        : numberInlet "r"
        : numberInlet "g"
        : numberInlet "b"
        : List.Nil
    , outletDefs :
        List.singleton $ colorOutlet "color"
    , process : R.FoldedByLabel foldToColor
    }
    where
        foldToColor (R.InletsMapData m) =
            R.OutletsMapData
                $ fromMaybe Map.empty
                $ buildColor <$> (m^.at "r") <*> (m^.at "g") <*> (m^.at "b")
        buildColor (Number' r) (Number' g) (Number' b) =
            Map.empty # Map.insert "color" (Color r g b)
        buildColor _ _ _ =
            Map.empty



metroNode :: Rpd.NodeDef Value
metroNode =
    { name : "metro"
    , inletDefs
        : triggerInlet "enabled"
        : periodInlet "period"
        : List.Nil
    , outletDefs :
        List.singleton $ bangOutlet "bang"
    , process : R.FoldedByLabel foldToBang
    }
    where
        foldToBang (R.InletsMapData m) =
            R.OutletsMapData
                $ fromMaybe Map.empty
                $ sendBang <$> (m^.at "enabled") <*> (m^.at "period")
        sendBang (Trigger isEnabled) (Period period) =
            if isEnabled then
                Map.empty # Map.insert "bang" Bang
            else Map.empty
        sendBang _ _ =
            Map.empty



-- TODO: metro, color, random, shape, magic, wind...
