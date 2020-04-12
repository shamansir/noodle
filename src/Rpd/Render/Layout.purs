module Rpd.Render.Layout where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Map as Map
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (fold, foldr)
import Data.Lens (view) as L

import Data.BinPack.R2 as R2

import Rpd.Path as Path
import Rpd.Util (Position, Rect, Bounds, type (/->), quickBounds', (+>))
import Rpd.Network as R
import Rpd.Optics (_networkPatches, _patchNodes) as L


type Layout d n =
    Path.ToPatch /-> PatchLayout d n


type PatchLayout d n =
    { offset :: Position
    , stack :: NodesStack d n
    , pinned :: PinnedNodes d n
    --, floating :: UUID.ToNode /-> Position
    }


data Cell d n
    = Abandoned
    | Taken (R.Node d n)


type NodesLayer d n = R2.Bin2 Number (Cell d n)
type NodesStack d n = Seq (NodesLayer d n) -- Shelving, may be?
type PinnedNodes d n = R.Node d n /-> ZIndex /\ Position


data LayerSize = LayerSize Rect
data NodeSize = NodeSize Rect
data Offset = Offset Position
data ZIndex = ZIndex Int


type GetNodeSize d n = R.Node d n -> NodeSize


layoutOf :: forall d n. Path.ToPatch -> Layout d n -> Maybe (PatchLayout d n)
layoutOf = Map.lookup


-- execute the function with the Stacks of the PatchLayout,
-- may be used to render the Stacks, where `x` is the `view`, then
withStackOf
    :: forall x d n
     . (Cell d n -> Position -> Rect -> x)
    -> PatchLayout d n
    -> Array x
withStackOf fn patchLayout =
    fold $ R2.unfold ((:) <<< fn') [] <$> patchLayout.stack
    where
        fn' :: (Cell d n /\ Number /\ Number /\ Number /\ Number) -> x
        fn' (cell /\ x /\ y /\ width /\ height ) =
            fn cell { x, y } { width, height }


-- execute the function with the pinned nodes of the PatchLayout,
-- may be used to render the pinned nodes, where `x` is the `view`, then
withPinnedOf
    :: forall x d n
     . (R.Node d n -> ZIndex /\ Position -> x)
    -> PatchLayout d n
    -> Array x
withPinnedOf fn patchLayout =
    uncurry fn <$> Map.toUnfoldable patchLayout.pinned


-- TODO: generic function : layoutOf + withStackOf + withPinnedOf


newLayer :: forall d n. LayerSize -> NodesLayer d n
newLayer (LayerSize { width, height }) =
    R2.container width height


-- load the existing nodes from the network into Stacks
loadIntoStacks
    :: forall d c n
     . LayerSize
    -> GetNodeSize d n
    -> R.Network d c n
    -> (Path.ToPatch /-> NodesStack d n)
loadIntoStacks layerSize getNodeSize nw =
    L.view L._networkPatches nw
        # map pairWithStack
        # Map.fromFoldable
    where
        pairWithStack (R.Patch patchUuid patchPath _) =
            patchPath /\
            (foldr packNode initialStack
                $ fromMaybe Seq.empty
                $ L.view (L._patchNodes patchUuid) nw
            )
        packNode node stack =
            packInStack layerSize (getNodeSize node) node stack
        initialStack =
            Seq.singleton $ newLayer layerSize


-- seems not to be used
-- unpackLayer :: forall d n. NodesLayer d n -> (R.Node d n /-> Bounds)
-- unpackLayer packing =
    -- Map.fromFoldable $ ((<$>) quickBounds') <$> R2.toList packing
    -- R2.unfold (\tuple map -> ) Map.empty packing


packInStack
    :: forall d n
     . LayerSize
    -> NodeSize
    -> R.Node d n
    -> NodesStack d n
    -> NodesStack d n
packInStack layerSize (NodeSize { width, height }) node stack =
    case Seq.head stack of
        Just topLayer ->
            case packNode topLayer of
                Just topLayerWithNode ->
                    (fromMaybe Seq.empty $ Seq.tail stack)
                        +> topLayerWithNode
                Nothing ->
                    -- failed to fit on the top layer
                    stack +> nodeOnNewLayer
        Nothing ->
            stack +> nodeOnNewLayer
    where
        packNode :: NodesLayer d n -> Maybe (NodesLayer d n)
        packNode layer =
            R2.packOne layer $ R2.item width height $ Taken node
        nodeOnNewLayer
            = newLayer layerSize # packNode # fromMaybe' (const $ newLayer layerSize)


newPatchLayout :: forall d n. Unit -> PatchLayout d n
newPatchLayout _ =
    { offset : { x : 0.0, y : 0.0 }
    , pinned : Map.empty
    , stack : Seq.empty
    }


-- TODO: internal, do not expose
modifyStack
    :: forall d n
     . Path.ToPatch
    -> (NodesStack d n -> NodesStack d n)
    -> Maybe (PatchLayout d n)
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
        newPLayout = newPatchLayout unit
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

resize
    :: forall d n
     . LayerSize
    -> Layout d n
    -> Layout d n
resize newSize layout =
    layout -- FIXME: implement, use R2.repack


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
            Map.insert patchPath (pinAtPatchLayout patchLayout) layout
        Nothing -> layout
    where
        pinAtPatchLayout patchLayout =
            let
                zIndex = ZIndex $ Map.size patchLayout.pinned
            in
                patchLayout
                    { pinned =
                        Map.insert node (zIndex /\ position) patchLayout.pinned
                    }


initWithStacks :: forall d n. (Path.ToPatch /-> NodesStack d n) -> Layout d n
initWithStacks patchToStack =
    patchToStack <#> \stack ->
        let patchLayout = newPatchLayout unit
        in patchLayout { stack = stack } -- TODO: _.stack


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



