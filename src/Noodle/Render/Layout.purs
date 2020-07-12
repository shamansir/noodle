module Noodle.Render.Layout where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map as Map

import Noodle.Path as Path
import Noodle.Util (Position, type (/->))
import Noodle.Network as R
import Noodle.Render.Component.Patch.Layout (Layout) as Patch
import Noodle.Render.Component.Patch.Layout
        (new, abandon, pinAt, modifyStack, packInStack) as PatchLayout
import Noodle.Render.Component.Patch.Layout (LayerSize, NodeSize, NodesStack)


type Layout d n =
    Path.ToPatch /-> Patch.Layout d n


-- FIXME: get rid of the functions below, they are just operations on the Map


layoutOf :: forall d n. Path.ToPatch -> Layout d n -> Maybe (Patch.Layout d n)
layoutOf = Map.lookup


modifyStack
    :: forall d n
     . Path.ToPatch
    -> (NodesStack d n -> NodesStack d n)
    -> Maybe (Patch.Layout d n)
    -> Layout d n
    -> Layout d n
modifyStack patchPath updateStack (Just patchLayout) layout =
    Map.insert patchPath
        (PatchLayout.modifyStack updateStack patchLayout)
        layout
modifyStack patchPath updateStack Nothing layout =
    let
        newPLayout = PatchLayout.new unit
    in
        Map.insert patchPath
            (PatchLayout.modifyStack updateStack newPLayout)
            layout


pack
    :: forall d c n
     . LayerSize
    -> NodeSize
    -> R.Patch d c n
    -> R.Node d n
    -> Layout d n
    -> Layout d n
pack layerSize nodeSize (R.Patch _ patchPath _) node layout =
    modifyStack
        patchPath
        (PatchLayout.packInStack layerSize nodeSize node)
        (Map.lookup patchPath layout)
        layout


pinAt
    :: forall d c n
     . R.Patch d c n
    -> R.Node d n
    -> Position
    -> Layout d n
    -> Layout d n
pinAt (R.Patch _ patchPath _) node position layout =
    case layout # layoutOf patchPath of
        Just patchLayout ->
            Map.insert patchPath (PatchLayout.pinAt node position patchLayout) layout
        Nothing -> layout


abandon :: forall d c n. R.Patch d c n -> R.Node d n -> Layout d n -> Layout d n
abandon (R.Patch _ patchPath _) node layout =
    case layout # layoutOf patchPath of
        Just patchLayout ->
            Map.insert patchPath (PatchLayout.abandon node patchLayout) layout
        Nothing -> layout


remove :: forall d c n. R.Patch d c n -> R.Node d n -> Layout d n -> Layout d n
remove = abandon



initWithStacks :: forall d n. (Path.ToPatch /-> NodesStack d n) -> Layout d n
initWithStacks patchToStack =
    patchToStack <#> \stack ->
        let patchLayout = PatchLayout.new unit
        in patchLayout { stack = stack } -- TODO: _.stack
