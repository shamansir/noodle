module Cli.Components.SidePanel where

import Prelude

import Data.Text.Output.Blessed (singleLine, multiLine) as T
import Data.Text.Format as T
import Data.String.CodeUnits as CU

import Type.Data.Symbol (class IsSymbol)
import Prim.Symbol (class Append)

import Effect (Effect)

import Control.Monad.State (get, modify_) as State

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
import Noodle.Ui.Cli.Tagging as T


type SidePanel (id :: Symbol) s =
    { char :: { on :: Char, off :: Char }
    , panelKey :: NodeKey Subj.Box id
    , buttonKey :: NodeKey Subj.Button id
    , init :: Array T.Tag
    , next :: (s -> Array T.Tag)
    , toggle :: (s -> s)
    }


panel
    :: forall id s. IsSymbol id => SidePanel id s -> C.Blessed s
panel sidePanel =
    B.boxAnd sidePanel.panelKey
        [ Box.width $ Dimension.calc $ Coord.percents 40.0 <-> Coord.px 5
        , Box.height $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 10
        , Box.top $ Offset.px 5
        , Box.left $ Offset.calc $ Coord.percents 60.0
        , Box.tags true
        , Box.content "."
        , Box.hidden true
        , Style.sidePanel
        , Style.sidePanelBorder
        ]
        [ ]
        $ const $ refresh sidePanel


button
    ∷ forall id s. IsSymbol id => Int -> SidePanel id s -> C.Blessed s
button offset sidePanel =
    B.button sidePanel.buttonKey
        [ Box.content $ T.singleLine $ T.buttonToggle (CU.singleton sidePanel.char.off) false
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px offset
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Box.tags true
        , Style.addPatch
        , Core.on Button.Press
            \_ _ -> do
                State.modify_ sidePanel.toggle
                sidePanel.panelKey >~ Element.toggle
                -- state <- State.get
                sidePanel.buttonKey >~ Box.setContent $ T.singleLine $ T.buttonToggle (CU.singleton sidePanel.char.on) true
                refresh sidePanel
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



refresh :: forall id s m. IsSymbol id => SidePanel id s -> BlessedOp s m
refresh sidePanel = do
    state <- State.get
    sidePanel.panelKey >~ Box.setContent $ T.multiLine $ T.stack $ sidePanel.next state
    -- Key.commandLogBox >~ Box.setContent $ NdfFile.toNdfCode state.commandLog
