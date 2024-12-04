module Cli.Components.NodeBox.InfoBox where

import Prelude


import Data.Symbol (class IsSymbol)
import Data.Text.Output.Blessed (singleLine) as T


import Blessed as B
import Blessed ((>~))

import Blessed.Internal.Core (Blessed) as Core
import Blessed.Internal.BlessedOp (BlessedOp) as C
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box

import Cli.Keys (InfoBoxKey)
import Cli.Style as Style
import Noodle.Ui.Cli.Tagging (inletInfoBox, outletInfoBox, removeInfoBox) as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel) as T

import Noodle.Id (InletR, OutletR) as Id


component:: forall state. InfoBoxKey -> Int -> Core.Blessed state
component key boxWidth =
    B.box key
        [ Box.top $ Offset.px 1
        , Box.left $ Offset.px 0
        -- , Box.width $ Dimension.calc $ C.px boxWidth <-> C.px 2 -- FIXME: didn't work
        , Box.width $ Dimension.px $ boxWidth - 2
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Box.content ""
        , Style.infoBox
        ]
        [ ]



{- familyStatus :: forall state f m. IsSymbol f => InfoBox -> Id.Family f -> C.BlessedOp state m
familyStatus family =
    Key.statusLine >~ Box.setContent $ T.singleLine $ T.nodeMouseOver family -}


inletInfo :: forall state m.  Id.InletR -> InfoBoxKey -> C.BlessedOp state m
inletInfo inletR infoBox =
    infoBox >~ Box.setContent $ T.singleLine $ T.inletInfoBox inletR


outletInfo :: forall state m. Id.OutletR -> InfoBoxKey -> C.BlessedOp state m
outletInfo outputId infoBox =
    infoBox >~ Box.setContent $ T.singleLine $ T.outletInfoBox outputId


removeInfo :: forall state m. InfoBoxKey -> C.BlessedOp state m
removeInfo infoBox =
    infoBox >~ Box.setContent $ T.singleLine $ T.removeInfoBox


clear :: forall state m. InfoBoxKey -> C.BlessedOp state m
clear infoBox =
    infoBox >~ Box.setContent ""
