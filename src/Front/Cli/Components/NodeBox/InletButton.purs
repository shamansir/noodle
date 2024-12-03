module Cli.Components.NodeBox.InletButton where

import Prelude

import Data.Maybe (Maybe)
import Data.Text.Output.Blessed (singleLine) as T

import Signal (Signal)

import Blessed as B

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Internal.Core as Core
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Base.Element.Method (show, focus) as Element

import Cli.Keys (InfoBoxKey, InletButtonKey, NodeBoxKey)
import Cli.State (State) {- LinkState(..), OutletIndex(..), InputIndex(..), logNdfCommandM)  -}
import Cli.Style (inputsOutputs) as Style

import Noodle.Ui.Cli.Tagging (inlet) as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel) as T


import Noodle.Id as Id
import Noodle.Patch (Patch)


--import Cli.Components.NodeBox.HasBody (class HasEditor, class HasEditor')



width :: Dimension
width = Dimension.px widthN


widthN :: Int
widthN = 3


left :: Int -> Offset
left idx = Offset.px $ idx * (widthN + 1)


component
    :: forall tk pstate fs repr m
     . T.At T.ChannelLabel repr
    => Patch pstate fs repr m
    -> InletButtonKey
    -> InfoBoxKey
    -> NodeBoxKey
    -> Id.InletR
    -> Int
    -> Maybe repr
    -> Signal repr
    -- -> Raw.Node
    -> Core.Blessed (State tk pstate fs repr m)
component curPatch buttonKey nextInfoBox nextNodeBox inletR idx mbRepr reprSignal =
    B.button buttonKey
        [ Box.content $ T.singleLine $ T.inlet idx inletR mbRepr
        , Box.top $ Offset.px 0
        , Box.left $ left idx
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width width
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inputsOutputs
        {-
        , Core.on Button.Press
            $ onPress curPatchId curPatch nextNodeBox idx pdin inode inputId $ Hydra.editorIdOf =<< maybeRepr
        , Core.on Element.MouseOver
            $ onMouseOver (Node.family inode) (Id.nodeIdR $ Node.id inode) nextNodeBox nextInfoBox idx inputId maybeRepr reprSignal
        , Core.on Element.MouseOut
            $ onMouseOut nextInfoBox idx
        -}
        ]
        []