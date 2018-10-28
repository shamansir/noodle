module Rpd.Renderer.String
    ( StringRenderer
    , stringRenderer
    , view -- TODO: do not expose maybe?
    ) where

import Prelude

import Data.Map as Map
import Data.Array as Array
import Data.List as List
import Data.Set as Set
import Data.Set (Set)
import Data.Lens as L
import Data.Either (Either(..))
import Data.String (joinWith)

import Rpd.Network
    ( Network(..)
    , Patch(..)
    , Node(..)
    , Inlet(..)
    , Outlet(..)
    , Link(..)
    ) as R
import Rpd.API (RpdError) as R
import Rpd.Optics (_node, _inlet, _outlet)
import Rpd.Render (PushMsg, Renderer(..))


type StringRenderer d = Renderer d String

data MultipleOrNone = MultipleOrNone String
data Single = Single String
data Counter = Counter MultipleOrNone Single


lineBreak = "\n" :: String
space = " " :: String


patchCounter = Counter (MultipleOrNone "Patches") (Single "Patch") :: Counter
nodeCounter = Counter (MultipleOrNone "Nodes") (Single "Node") :: Counter
inletCounter = Counter (MultipleOrNone "Inlets") (Single "Inlet") :: Counter
outletCounter = Counter (MultipleOrNone "Outlets") (Single "Outlet") :: Counter
linkCounter = Counter (MultipleOrNone "Links") (Single "Link") :: Counter


count :: Counter -> Int -> String
count (Counter (MultipleOrNone none) _ ) size    | size == 0 = "No " <> none
count (Counter _ (Single single)) size           | size == 1 = "One " <> single
count (Counter (MultipleOrNone multiple) _) size | size > 1  = show size <> " " <> multiple
count (Counter (MultipleOrNone multiple) _) _    | otherwise = "? " <> show multiple


stringRenderer :: forall d. StringRenderer d
stringRenderer =
    Renderer "" view



view :: forall d. PushMsg d -> Either R.RpdError (R.Network d) -> String
view pushMsg (Right nw@(R.Network { name } { patches, links })) =
    "Network " <> name <> ":" <> lineBreak
        <> count patchCounter patchCount
        <> (if patchCount > 0 then
                lineBreak <> space <> patchesInfo
            else "")
        <> lineBreak <> count linkCounter linkCount
        <> (if linkCount > 0 then
                lineBreak <> space <> linksInfo
            else "")
    where
        patchCount = Map.size patches
        linkCount = Map.size links
        patchesInfo =
            joinWith (lineBreak <> space)
                $ (viewPatch nw <$> Map.values patches)
                    # List.toUnfoldable
        linksInfo =
            joinWith lineBreak
                $ (viewLink nw <$> Map.values links)
                    # List.toUnfoldable
view pushMsg (Left err) =
    "<" <> show err <> ">"


viewPatch :: forall d. R.Network d -> R.Patch d -> String
viewPatch nw (R.Patch id { name } { nodes }) =
    "Patch " <> name <> " " <> show id <> ":" <> lineBreak <> space
        <> count nodeCounter (Set.size nodes) <> lineBreak <> space <> space
        <> nodesInfo
    where
        allNodes =
            Set.toUnfoldable nodes # map \path -> L.view (_node path) nw
        nodesInfo =
            joinWith (lineBreak <> space <> space)
                $ viewNode nw <$> Array.catMaybes allNodes


viewNode :: forall d. R.Network d -> R.Node d -> String
viewNode nw (R.Node path { name } { inlets, outlets }) =
    "Node " <> name <> " " <> show path <> ":" <> lineBreak
        <> twiceSpace <> count inletCounter inletCount
        <> (if inletCount > 0 then
                lineBreak <> tripleSpace <> inletsInfo
            else "")
        <> lineBreak <> twiceSpace <> count outletCounter outletCount
        <> (if outletCount > 0 then
                lineBreak <> tripleSpace <> outletsInfo
            else "")
    where
        inletCount = Set.size inlets
        outletCount = Set.size outlets
        twiceSpace = space <> space
        tripleSpace = space <> space <> space
        allInlets =
            Set.toUnfoldable inlets # map \path -> L.view (_inlet path) nw
        inletsInfo =
            joinWith (lineBreak <> tripleSpace)
                $ viewInlet nw <$> Array.catMaybes allInlets
        allOutlets =
            Set.toUnfoldable outlets # map \path -> L.view (_outlet path) nw
        outletsInfo =
            joinWith (lineBreak <> tripleSpace)
                $ viewOutlet nw <$> Array.catMaybes allOutlets


viewInlet :: forall d. R.Network d -> R.Inlet d -> String
viewInlet _ (R.Inlet path { label } _) =
    "Inlet " <> label <> " " <> show path


viewOutlet :: forall d. R.Network d -> R.Outlet d -> String
viewOutlet _ (R.Outlet path { label } _) =
    "Outlet " <> label <> " " <> show path


viewLink :: forall d. R.Network d -> R.Link -> String
viewLink _ (R.Link outletPath inletPath) =
    "Link from " <> show outletPath <> " to " <> show inletPath


-- collectSetInfo nw viewItem lens source =
--     joinWith (lineBreak <> space)
--         $ viewItem nw <$> Array.catMaybes allItems
--     where
--         allItems = Set.toUnfoldable source # map \path -> L.view (lens path) nw
