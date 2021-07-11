module App.Style.Quartz.Units
    (units) where


import Prelude

import App.Style (Units, NodeFlow(..))


{- data Size a = Size Int Int a


data Pixels = Pixels


data Cells = Cells -}


units :: NodeFlow -> Units


units flow =
    { cellWidth
    , cellHeight
    , nodeBodyWidth : nodeBodyWidth flow
    , nodeBodyHeight : nodeBodyHeight flow
    , namePlateHeight : namePlateHeight flow
    , namePlateWidth : namePlateWidth flow
    , slotOuterWidth : slotOuterWidth flow
    , slotOuterHeight : slotOuterHeight flow
    , nodePadding : nodePadding flow
    , slotRadius
    , slotStrokeWidth
    , bodyStrokeWidth
    , bodyCornerRadius
    , bodyShadowShift
    }


cellWidth = 40.0


cellHeight = 40.0


nodeBodyWidth _ = cellWidth * 2.0


nodeBodyHeight _ = cellHeight * 2.0


nodePadding _ = 20.0


namePlateHeight _ = 15.0


namePlateWidth _ = 15.0


slotOuterWidth _ = 50.0


slotOuterHeight _ = 25.0


slotRadius = 4.0


slotStrokeWidth = 1.5


bodyStrokeWidth = 1.0


bodyCornerRadius = 0.0


bodyShadowShift = 5.0