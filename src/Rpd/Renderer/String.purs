module Rpd.Renderer.String
    ( StringRenderer
    , stringRenderer
    , view -- TODO: do not expose maybe?
    ) where

import Prelude

import Data.Map as Map
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.List as List
import Data.List (List)
import Data.Set as Set
import Data.Set (Set)
import Data.Lens as L
import Data.Lens.At (at)
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
import Rpd.Optics (_node)
import Rpd.Render (PushMsg, Renderer(..))


type StringRenderer d = Renderer d String

data MultipleOrNone = MultipleOrNone String
data Single = Single String
data Counter = Counter MultipleOrNone Single


lineBreak = "\r\n" :: String
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


viewNode :: forall d. R.Node d -> String
viewNode (R.Node _ _ { inlets, outlets }) =
    show $ Set.size inlets


viewPatch :: forall d. R.Network d -> R.Patch d -> String
viewPatch nw (R.Patch id { name } { nodes }) =
    "Patch " <> name <> " " <> show id <> ":" <> lineBreak
        <> count nodeCounter (Set.size nodes) <> ""
        <> nodesInfo
    where
        (allNodes :: Array (Maybe (R.Node d))) =
            Set.toUnfoldable nodes # map (\path -> L.view (_node path) nw)
        nodesInfo =
            joinWith (lineBreak <> space)
                $ (viewNode <$> Array.catMaybes allNodes)


view :: forall d. PushMsg d -> Either R.RpdError (R.Network d) -> String
view pushMsg (Right nw@(R.Network { name } { patches })) =
    "Network " <> name <> ":" <> lineBreak
        <> count patchCounter (Map.size patches) <> ""
        <> patchesInfo
    where
        patchesInfo =
            joinWith (lineBreak <> space)
                $ (viewPatch nw <$> Map.values patches)
                    # List.toUnfoldable
view pushMsg (Left err) =
    "<" <> show err <> ">"
