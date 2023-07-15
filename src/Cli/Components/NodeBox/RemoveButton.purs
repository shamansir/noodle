module Cli.Components.NodeBox.RemoveButton where

import Prelude


import Data.SProxy (reflect, reflect')
import Data.Symbol (class IsSymbol)

import Blessed as B

import Blessed ((>~))
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Tagger (render) as T

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box
import Blessed.Internal.Core as Core
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys (RemoveButtonKey, InfoBoxKey)
import Cli.Keys (mainScreen, statusLine) as Key
import Cli.Style as Style
import Cli.Tagging as T

import Noodle.Id as Id


component :: forall f state. IsSymbol f => Id.Family f -> InfoBoxKey -> RemoveButtonKey -> Core.Blessed state
component family infoBoxKey buttonKey =
    B.button buttonKey
        [ Box.content $ T.render $ T.removeButtonOut
        , Box.top $ Offset.px $ 3
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 3
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inletsOutlets
        , Core.on Button.Press
            \_ _ -> pure unit --TODO
        , Core.on Element.MouseOver
            \_ _ -> do
                buttonKey >~ Box.setContent $ T.render $ T.removeButtonOver
                infoBoxKey >~ Box.setContent $ T.render $ T.removeInfoBox
                Key.statusLine >~ Box.setContent $ T.render $ T.removeStatusLine family
                Key.mainScreen >~ Screen.render -- FIXME: refresh only the area
        , Core.on Element.MouseOut
            \_ _ -> do
                buttonKey >~ Box.setContent $ T.render $ T.removeButtonOut
                -- Info box : delete this node
                infoBoxKey >~ Box.setContent ""
                Key.statusLine >~ Box.setContent ""
                Key.mainScreen >~ Screen.render -- FIXME: refresh only the area
        ]
        []