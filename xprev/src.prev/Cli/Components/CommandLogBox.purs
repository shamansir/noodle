module Cli.Components.CommandLogBox where

import Prelude

import Data.Text.Output.Blessed (singleLine) as T

import Effect (Effect)

import Control.Monad.State (get) as State

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.BlessedOp (BlessedOp)

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box

import Cli.Keys (commandLogBox) as Key
import Cli.Style as Style
import Cli.State (State)
import Cli.Components.NodeBox.InputButton as InputButton

import Noodle.Text.NdfFile (toNdfCode, toTaggedNdfCode) as NdfFile


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (InputButton.widthN + 1) * count


component
    :: C.Blessed State
component =
    B.boxAnd Key.commandLogBox
        [ Box.width $ Dimension.calc $ Coord.percents 40.0 <-> Coord.px 5
        , Box.height $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 10
        , Box.top $ Offset.px 5
        , Box.left $ Offset.calc $ Coord.percents 60.0
        , Box.tags true
        , Box.content "."
        , Box.hidden true
        -- , List.items is


        -- , ListBar.commands $ mapWithIndex (\idx hiinr -> Node.withInputInNodeMRepr hiinr (inputHandler curPatchId curPatch nextNodeBox idx)) is


        -- , ListBar.commands $ List.toUnfoldable $ mapWithIndex inputHandler $ is
        -- , List.mouse true
        -- , List.keys true
        -- , ListBar.autoCommandKeys true
        , Style.commandLog
        , Style.patchBoxBorder
        {- , Core.on ListBar.Select
            \_ _ -> do
                liftEffect $ Console.log "input"
                inputSelected <- List.selected ~< nextInputsBox
                liftEffect $ Console.log $ show inputSelected
        -}
        ]
        [ ]
        $ const refresh


refresh :: BlessedOp State Effect
refresh = do
    state <- State.get
    Key.commandLogBox >~ Box.setContent $ T.singleLine $ NdfFile.toTaggedNdfCode state.commandLog
    -- Key.commandLogBox >~ Box.setContent $ NdfFile.toNdfCode state.commandLog
