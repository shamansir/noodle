module Cli.Components.HydraCodeBox where

import Prelude

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

import Cli.Keys (hydraCodeBox) as Key
import Cli.Style as Style
import Cli.State (State)
import Cli.Components.NodeBox.InletButton as InletButton

import Toolkit.Hydra2.Lang (formProgram) as Lang
import Toolkit.Hydra2.Lang.ToCode (toCode, javaScript) as Lang


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (InletButton.widthN + 1) * count


component
    :: C.Blessed State
component =
    B.boxAnd Key.hydraCodeBox
        [ Box.width $ Dimension.calc $ Coord.percents 40.0 <-> Coord.px 5
        , Box.height $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 10
        , Box.top $ Offset.px 5
        , Box.left $ Offset.calc $ Coord.percents 60.0
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
    Key.hydraCodeBox >~ Box.setContent $ Lang.toCode Lang.javaScript $ Lang.formProgram state.program