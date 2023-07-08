module Cli.Components.CommandLogBox where

import Prelude

import Effect (Effect)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe)
import Data.Array (length, zip) as Array
import Data.Map (Map)
import Data.Map as Map

import Control.Monad.State (get) as State

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.NodeKey (nestChain) as NK
import Blessed.Internal.BlessedOp (BlessedOp)

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method as Box

import Cli.Keys (commandLogBox) as Keys
import Cli.Style as Style
import Cli.State (State)
import Cli.Components.NodeBox.InletButton as InletButton

import Noodle.Id as Id
import Noodle.Node2 as Node
import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch
import Noodle.Family.Def as Family
import Noodle.Text.NetworkFile.Command (commandsToNdf)

import Toolkit.Hydra2 (Instances, State) as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (InletButton.widthN + 1) * count


component
    :: C.Blessed State
component =
    B.boxAnd Keys.commandLogBox
        [ Box.width $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 10
        , Box.height $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 10
        , Box.top $ Offset.px 5
        , Box.left $ Offset.px 5
        , Box.tags true
        , Box.content "."
        , Box.hidden true
        -- , List.items is


        -- , ListBar.commands $ mapWithIndex (\idx hiinr -> Node.withInputInNodeMRepr hiinr (inletHandler curPatchId curPatch nextNodeBox idx)) is


        -- , ListBar.commands $ List.toUnfoldable $ mapWithIndex inletHandler $ is
        -- , List.mouse true
        -- , List.keys true
        -- , ListBar.autoCommandKeys true
        , Style.commandLog
        , Style.patchBoxBorder
        {- , Core.on ListBar.Select
            \_ _ -> do
                liftEffect $ Console.log "inlet"
                inletSelected <- List.selected ~< nextInletsBox
                liftEffect $ Console.log $ show inletSelected
        -}
        ]
        [ ]
        $ const refresh


refresh :: BlessedOp State Effect
refresh = do
    state <- State.get
    Keys.commandLogBox >~ Box.setContent $ commandsToNdf state.commandLog
