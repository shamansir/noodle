module Noodle.Render.Layout where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Map as Map
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Sequence.Extra ((+>))
import Data.Sequence.Extra (catMaybes) as Seq
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (fold, foldr)
import Data.Lens (view, preview, _Just) as L

import Data.BinPack.R2 as R2

import Noodle.Path as Path
import Noodle.Util (Position, Rect, type (/->))
import Noodle.Network as R
import Noodle.Optics as L
import Noodle.Render.Component.Patch.Layout (Layout) as Patch


type Layout d n =
    Path.ToPatch /-> Patch.Layout d n


layoutOf :: forall d n. Path.ToPatch -> Layout d n -> Maybe (Patch.Layout d n)
layoutOf = Map.lookup



modifyStack
    :: forall d n
     . Path.ToPatch
    -> (NodesStack d n -> NodesStack d n)
    -> Maybe (Layout d n)
    -> Layout d n
    -> Layout d n
modifyStack patchPath updateStack (Just patchLayout) layout =
    Map.insert patchPath
        (patchLayout
            { stack = updateStack patchLayout.stack
            }
        )
        layout
modifyStack patchPath updateStack Nothing layout =
    let
        newPLayout = newLayout unit
    in
        Map.insert patchPath
            (newPLayout
                { stack = updateStack newPLayout.stack
                }
            )
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
        (packInStack layerSize nodeSize node)
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
            Map.insert patchPath (pinAtLayout patchLayout) layout
        Nothing -> layout
    where
        pinAtLayout patchLayout =
            let
                zIndex = ZIndex $ Map.size patchLayout.pinned
            in
                patchLayout
                    { pinned =
                        Map.insert node (zIndex /\ position) patchLayout.pinned
                    }


abandon :: forall d c n. R.Patch d c n -> R.Node d n -> Layout d n -> Layout d n
abandon (R.Patch _ patchPath _) node layout =
    case layout # layoutOf patchPath of
        Just patchLayout ->
            Map.insert patchPath (removeFromPatchLayout patchLayout) layout
        Nothing -> layout
    where
        freeNode (Taken n) | n == node = Abandoned
        freeNode (Taken n) | otherwise = Taken n
        freeNode v = v
        removeFromPatchLayout patchLayout =
            patchLayout
                { stack =
                    (<$>) freeNode <$> patchLayout.stack
                , pinned =
                    patchLayout.pinned # Map.delete node
                }


remove :: forall d c n. R.Patch d c n -> R.Node d n -> Layout d n -> Layout d n
remove = abandon



initWithStacks :: forall d n. (Path.ToPatch /-> NodesStack d n) -> Layout d n
initWithStacks patchToStack =
    patchToStack <#> \stack ->
        let patchLayout = newLayout unit
        in patchLayout { stack = stack } -- TODO: _.stack
