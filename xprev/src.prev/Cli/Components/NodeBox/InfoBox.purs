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
import Cli.Tagging as T

import Noodle.Id (Input, Output) as Id


component:: forall state. InfoBoxKey -> Int → Core.Blessed state
component nextInfoBox boxWidth =
    B.box nextInfoBox
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


inputInfo :: forall state i m. IsSymbol i => Id.Input i -> InfoBoxKey -> C.BlessedOp state m
inputInfo inputId infoBox =
    infoBox >~ Box.setContent $ T.singleLine $ T.inputInfoBox inputId


outputInfo :: forall state o m. IsSymbol o => Id.Output o -> InfoBoxKey -> C.BlessedOp state m
outputInfo outputId infoBox =
    infoBox >~ Box.setContent $ T.singleLine $ T.outputInfoBox outputId


removeInfo :: forall state m. InfoBoxKey -> C.BlessedOp state m
removeInfo infoBox =
    infoBox >~ Box.setContent $ T.singleLine $ T.removeInfoBox


clear :: forall state m. InfoBoxKey -> C.BlessedOp state m
clear infoBox =
    infoBox >~ Box.setContent ""
