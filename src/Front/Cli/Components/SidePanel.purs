module Cli.Components.SidePanel where

import Prelude

import Data.Text.Output.Blessed (singleLine, multiLine) as T
import Data.Text.Format as T
import Data.String.CodeUnits as CU
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)

import Type.Data.Symbol (class IsSymbol)
import Prim.Symbol (class Append)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Control.Monad.State (get, modify) as State

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.BlessedSubj as Subj

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.Core (on) as Core
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (lift') as Blessed

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Method (toggle) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys (mainScreen) as Key
import Cli.Style as Style
import Cli.State (State)

import Noodle.Text.NdfFile (toNdfCode, toTaggedNdfCode) as NdfFile
import Noodle.Ui.Tagging as T


type SidePanel (id :: Symbol) s v = -- FIXME: `s` should be the actual state of the SidePanel: is it visible and its content
    { title :: String
    , char :: v -> Char
    , isOn :: v -> Boolean
    , panelKey :: NodeKey Subj.Log id
    , buttonKey :: NodeKey Subj.Button id
    , next :: (s -> Effect (v /\ Array T.Tag))
    , onToggle :: (s -> s)
    }


panel
    :: forall id s v. IsSymbol id => v /\ Array T.Tag -> SidePanel id s v -> C.Blessed s
panel initial sidePanel =
    B.logAnd sidePanel.panelKey
        [ Box.width $ Dimension.calc $ Coord.percents 40.0 <-> Coord.px 5
        , Box.height $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 5
        , Box.top $ Offset.px 3
        , Box.left $ Offset.calc $ Coord.percents 60.0
        , Box.tags true
        , Box.content "."
        , Box.hidden true
        , Box.label sidePanel.title
        , Box.scrollable true
        -- , Box.alwaysScroll true
        , Style.sidePanel
        , Style.sidePanelBorder
        ]
        [ ]
        $ const $ refreshWith initial sidePanel


button
    ∷ forall id s v. IsSymbol id => Int -> v -> SidePanel id s v -> C.Blessed s
button offset initV sidePanel =
    B.button sidePanel.buttonKey
        [ Box.content $ T.singleLine $ T.buttonToggle (CU.singleton $ sidePanel.char initV) $ sidePanel.isOn initV
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px offset
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Box.tags true
        , Style.addPatch
        , Core.on Button.Press
            \_ _ -> toggle sidePanel
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


refresh :: forall id s m v. MonadEffect m => IsSymbol id => SidePanel id s v -> BlessedOp s m
refresh sidePanel = do
    state <- State.get
    next <- liftEffect $ sidePanel.next state
    sidePanel # refreshWith next


refreshWith :: forall id s m v. IsSymbol id => v /\ Array T.Tag -> SidePanel id s v -> BlessedOp s m
refreshWith (nextV /\ nextContent) sidePanel = do
    sidePanel.panelKey  >~ Box.setContent $ T.multiLine  $ T.stack nextContent
    sidePanel.buttonKey >~ Box.setContent $ T.singleLine $ T.buttonToggle (CU.singleton $ sidePanel.char nextV) $ sidePanel.isOn nextV
    -- Key.mainScreen >~ Screen.render


toggle :: forall id s m v. MonadEffect m => IsSymbol id => SidePanel id s v -> BlessedOp s m
toggle sidePanel = do
    state <- State.modify sidePanel.onToggle
    next <- liftEffect $ sidePanel.next state
    sidePanel # refreshWith next
    sidePanel.panelKey >~ Element.toggle
    Key.mainScreen >~ Screen.render