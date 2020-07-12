module Noodle.Render.Component.Patch.Layout where


import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Map as Map
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.BinPack.R2 as R2
import Data.Sequence (empty, head, singleton, tail) as Seq
import Data.Sequence (Seq)
import Data.Sequence.Extra ((+>))
import Data.Sequence.Extra (catMaybes) as Seq
import Data.Foldable (fold, foldr)
import Data.Lens (view, _Just) as L

import Noodle.Path as Path
import Noodle.Network as R
import Noodle.Optics (_node, _patch, _patchNodes, _patches) as L
import Noodle.Util (Rect, Position, type (/->))

-- TODO: generalize to any cell

type Layout d n =
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


-- execute the function with the Stacks of the Layout,
-- may be used to render the Stacks, where `x` is the `view`, then
withStackOf
    :: forall x d n
     . (Cell d n -> Position -> Rect -> x)
    -> Layout d n
    -> Array x
withStackOf fn patchLayout =
    fold $ R2.unfold ((:) <<< fn') [] <$> patchLayout.stack
    where
        fn' :: (Cell d n /\ Number /\ Number /\ Number /\ Number) -> x
        fn' (cell /\ x /\ y /\ width /\ height ) =
            fn cell { x, y } { width, height }


-- execute the function with the pinned nodes of the Layout,
-- may be used to render the pinned nodes, where `x` is the `view`, then
withPinnedOf
    :: forall x d n
     . (R.Node d n -> ZIndex /\ Position -> x)
    -> Layout d n
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
    L.view (L._patches) nw
        <#> (\uuid -> L.view (L._patch uuid) nw)
         # Seq.catMaybes
        <#> pairWithStack
         # Map.fromFoldable
    where
        pairWithStack (R.Patch patchUuid patchPath _) =
            patchPath /\
            (foldr packNode initialStack
                 $ Seq.catMaybes
                 $ (\nodeUuid -> L.view (L._node nodeUuid) nw)
                <$> L.view (L._patch patchUuid <<< L._Just <<< L._patchNodes) nw
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


new :: forall d n. Unit -> Layout d n
new _ =
    { offset : { x : 0.0, y : 0.0 }
    , pinned : Map.empty
    , stack : Seq.empty
    }


-- TODO: internal, do not expose
modifyStack
    :: forall d n
     . (NodesStack d n -> NodesStack d n)
    -> Layout d n
    -> Layout d n
modifyStack updateStack layout =
    layout
        { stack = updateStack layout.stack
        }


pack
    :: forall d n
     . LayerSize
    -> NodeSize
    -> R.Node d n
    -> Layout d n
    -> Layout d n
pack layerSize nodeSize node layout =
    modifyStack
        (packInStack layerSize nodeSize node)
        layout


resize
    :: forall d n
     . LayerSize
    -> Layout d n
    -> Layout d n
resize newSize layout =
    layout -- FIXME: implement, use R2.repack


pinAt
    :: forall d n
     . R.Node d n
    -> Position
    -> Layout d n
    -> Layout d n
pinAt node position layout =
    let
        zIndex = ZIndex $ Map.size layout.pinned
    in
        layout
            { pinned =
                Map.insert node (zIndex /\ position) layout.pinned
            }


abandon :: forall d n. R.Node d n -> Layout d n -> Layout d n
abandon node layout =
    layout
        { stack =
            (<$>) freeNode <$> layout.stack
        , pinned =
            layout.pinned # Map.delete node
        }
    where
        freeNode (Taken n) | n == node = Abandoned
        freeNode (Taken n) | otherwise = Taken n
        freeNode v = v


remove :: forall d n. R.Node d n -> Layout d n -> Layout d n
remove = abandon
