module Cli.Components.HydraCodeButton where


import Prelude

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Core.Dimension as Dimension
import Blessed.Internal.Core as Core

import Blessed.UI.Boxes.Box.Option (content, height, left, top, width) as Box

import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Method (toggle) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style as Style

import Cli.Components.HydraCodeBox as HydraCodeBox


component ∷ Core.Blessed State
component =
    B.button Key.hydraCodeButton
        [ Box.content "H"
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 7
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Style.addPatch
        , Core.on Button.Press
            \_ _ -> do
                Key.hydraCodeBox >~ Element.toggle
                HydraCodeBox.refresh
                Key.mainScreen >~ Screen.render
        {-
        , Core.on Element.MouseOver
            \_ _ -> do
                liftEffect $ Console.log "over"
        , Core.on Element.MouseOut
            \_ _ -> do
                liftEffect $ Console.log "out"
        -}
        ]
        []
