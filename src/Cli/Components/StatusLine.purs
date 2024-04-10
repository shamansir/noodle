module Cli.Components.StatusLine where

import Prelude

import Effect (Effect)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe)
import Data.Array (length, zip) as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Symbol (class IsSymbol)
import Data.Text.Format as T
import Data.Text.Output.Blessed (render) as T

import Blessed ((>~))
import Blessed as B

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.BlessedOp (BlessedOp) as C
import Blessed.Internal.NodeKey (nestChain) as NK

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method as Box

import Cli.Keys (statusLine) as Key
import Cli.Style as Style
import Cli.State (State)
import Cli.Tagging as T

-- import Cli.Components.NodeBox.InputButton as InputButton

import Noodle.Id as Id
import Noodle.Node as Node
import Noodle.Patch (Patch)
import Noodle.Patch as Patch
import Noodle.Family.Def as Family

import Toolkit.Hydra (Instances, State) as Hydra
import Toolkit.Hydra.Repr.Wrap (WrapRepr) as Hydra


{-}
width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (InputButton.widthN + 1) * count -}


component
    :: C.Blessed State
component =
    B.box Key.statusLine
        [ Box.width $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 2
        , Box.height $ Dimension.px 1
        , Box.top $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 2
        , Box.left $ Offset.px 1
        , Box.tags true
        , Box.content ""
        -- , List.items is


        -- , ListBar.commands $ mapWithIndex (\idx hiinr -> Node.withInputInNodeMRepr hiinr (inputHandler curPatchId curPatch nextNodeBox idx)) is


        -- , ListBar.commands $ List.toUnfoldable $ mapWithIndex inputHandler $ is
        -- , List.mouse true
        -- , List.keys true
        -- , ListBar.autoCommandKeys true
        , Style.statusLine
        {- , Core.on ListBar.Select
            \_ _ -> do
                liftEffect $ Console.log "input"
                inputSelected <- List.selected ~< nextInputsBox
                liftEffect $ Console.log $ show inputSelected
        -}
        ]
        [ ]


familyStatus :: forall state f m. IsSymbol f => Id.Family f -> C.BlessedOp state m
familyStatus family =
    Key.statusLine >~ Box.setContent $ T.render $ T.nodeMouseOver family


inputStatus :: forall state f i m. IsSymbol f => IsSymbol i => Id.Family' f -> Int -> Id.Input i -> Maybe Hydra.WrapRepr -> C.BlessedOp state m
inputStatus family idx inputId maybeRepr =
    Key.statusLine >~ Box.setContent $ T.render $ T.inputStatusLine family idx inputId maybeRepr


outputStatus :: forall state f o m. IsSymbol f => IsSymbol o => Id.Family' f -> Int -> Id.Output o -> Maybe Hydra.WrapRepr -> C.BlessedOp state m
outputStatus family idx outputId maybeRepr =
    Key.statusLine >~ Box.setContent $ T.render $ T.outputStatusLine family idx outputId maybeRepr


removeStatus :: forall state f m. IsSymbol f ⇒ Id.Family f -> C.BlessedOp state m
removeStatus family =
    Key.statusLine >~ Box.setContent $ T.render $ T.removeStatusLine family


clear :: forall state m. C.BlessedOp state m
clear =
    Key.statusLine >~ Box.setContent ""