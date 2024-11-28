module Cli.Components.NodeBox.InletButton where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Control.Monad.State (get, modify, modify_) as State

import Data.Maybe (Maybe(..))
import Noodle.Repr (class FromRepr, class ToRepr, toRepr, fromRepr, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map as Map
import Data.Text.Format as T
import Data.Text.Output.Blessed (singleLine) as T

import Type.Proxy (Proxy(..))
import Signal (Signal)
import Signal (get) as Signal

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Internal.Core as Core
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.NodeKey (rawify) as NodeKey
import Blessed.Internal.BlessedSubj (Line)
import Blessed.UI.Boxes.Box.Option (content, height, left, style, top, width) as Box
import Blessed.UI.Base.Element.PropertySet (setTop, setLeft) as Element
import Blessed.UI.Base.Element.Method (setFront) as Element
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Forms.TextArea.Method (setValue) as TextArea
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Base.Element.Method (show, focus) as Element

import Cli.Keys as Key
import Cli.Keys (NodeBoxKey, PatchBoxKey, InfoBoxKey, InletButtonKey, mainScreen, statusLine)
import Cli.State (State) {- LinkState(..), OutletIndex(..), InputIndex(..), logNdfCommandM)  -}
import Cli.Bounds (collect, inputPos) as Bounds
import Cli.Style (inputsOutputs) as Style
import Noodle.Ui.Cli.Tagging as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel) as T
import Noodle.Ui.Cli.Palette.Mark (class Mark)

import Noodle.Network as Network
import Noodle.Patch as Patch

import Noodle.Id as Id
import Noodle.Node (Node) as Noodle
import Noodle.Node as Node
import Noodle.Patch (Patch)
import Noodle.Text.NdfFile (toNdfCode, toTaggedNdfCode) as NdfFile
import Noodle.Text.NdfFile.Command as Cmd
import Noodle.Text.NdfFile.Command as C
import Noodle.Text.NdfFile.Command (commandsToNdf)



--import Cli.Components.NodeBox.HasBody (class HasEditor, class HasEditor')



width :: Dimension
width = Dimension.px widthN


widthN :: Int
widthN = 3


left :: Int -> Offset
left idx = Offset.px $ idx * (widthN + 1)


component
    :: forall tk pstate fs repr m
     . Mark repr
    => T.At T.ChannelLabel repr
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
        [ Box.content $ T.singleLine $ T.inlet' idx inletR mbRepr
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