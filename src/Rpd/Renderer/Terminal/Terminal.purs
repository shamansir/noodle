module Rpd.Renderer.Terminal
    ( TerminalRenderer
    , terminalRenderer
    , Ui
    , View
    , Msg
    , Packing -- TODO: do not expose maybe?
    , Status -- TODO: do not expose maybe?
    , view -- TODO: do not expose maybe?
    ) where

import Prelude

import Control.Alt ((<|>))

import Data.Array ((!!))
import Data.Array as Array
import Data.Int (toNumber, floor, round)
import Data.List (List, (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Sequence as Seq
import Data.String (CodePoint, fromCodePointArray, toCodePointArray, codePointFromChar, joinWith)
import Data.String as String
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Newtype (unwrap)

import Data.Either (Either(..))

import Data.Foldable (foldMap, foldr, sequence_)
import Data.Lens as Lens
import Data.Traversable (traverse)

import Data.BinPack.R2 as R2

import Math (ceil, sqrt, (%))

import Rpd.API.Errors (RpdError) as R
import Rpd.API.Action as C
import Rpd.Network (Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)) as R
import Rpd.Optics as R
import Rpd.Path as R
import Rpd.Render.MUV (Renderer(..), PushF(..), skipEffects) as R
import Rpd.Toolkit as T

import Rpd.Renderer.Terminal.Multiline as ML


data Subject
    = PatchSubj R.ToPatch
    | NodeSubj R.ToNode
    | InletSubj R.ToInlet
    | OutletSubj R.ToOutlet


instance showSubject :: Show Subject where
    show (PatchSubj patchPath) = show patchPath
    show (NodeSubj nodePath) = show nodePath
    show (InletSubj inletPath) = show inletPath
    show (OutletSubj outletPath) = show outletPath

-- data Cell =
--     Cell R.Path View

-- R2.Bin2 Stores information about width/height and x/y
data Packing = Packing (R2.Bin2 Int { subject :: Subject, packing :: Maybe Packing })

type Item = R2.Item Int { subject :: Subject, packing :: Maybe Packing }


data Status
    = Empty
    | Error (R.RpdError)
    | WritingCommand String
    -- TODO : Selection


type Ui =
    -- { packing : Packing
    { status :: Status
    , invalidate :: Boolean
    }


data Msg
    = Skip
    | ExecuteCommand String
    | ClickAt (Int /\ Int)
    | Receive Char


type View = ML.Multiline


type Bounds = Int /\ Int
type Pos = Int /\ Int


initialBounds :: Bounds
initialBounds = 20 /\ 20


initUi :: Ui
initUi =
    -- { packing : Packing $ R2.container 200 200
    { status : Empty
    , invalidate : true
    }


type TerminalRenderer d c n  = R.Renderer d c n Ui View Msg Unit


terminalRenderer :: forall d c n. TerminalRenderer d c n
terminalRenderer =
    R.Renderer
        { from : ML.empty
        , init : const initUi
        , update : const update
        , view
        , performEffect : R.skipEffects
        }


noView :: View
noView = ML.empty


-- viewPacking :: Packing -> Network -> View
-- viewPacking (Packing r2) nw =
--     fromMaybe noView $
--         R2.valueOf r2 >>=
--             \{ cell } ->
--                 case cell of
--                     (Cell _ view) -> pure view

    -- TODO: make Foldable instance
    -- fromMaybe "[]" $ foldConcat <$> R2.valueOf r2
    -- where
    --     foldCell :: Cell -> View
    --     foldCell (Cell _ content) = foldr (<>) "" $ fromCodePointArray <$> content
    --     foldPacking' :: Maybe Packing -> View
    --     foldPacking' packing = packing >>= pure <<< R2.toList # fromMaybe ""
    --     foldConcat { cell, packing } = foldCell cell <> foldPacking' packing


viewStatus :: Status -> View
viewStatus _ = ML.from' ">"


packInlet :: forall d c n. R.Network d c n -> R.Inlet d c -> Item
packInlet nw (R.Inlet _ path _ _) =
    R2.item 0 0 { subject : InletSubj path, packing : Nothing }


packOutlet :: forall d c n. R.Network d c n -> R.Outlet d c -> Item
packOutlet nw (R.Outlet _ path _ _) =
    R2.item 0 0 { subject : OutletSubj path, packing : Nothing }


viewNode :: forall d c n. R.Network d c n -> R.Node d n -> View
viewNode nw (R.Node uuid path@(R.ToNode { node : name }) _ _ { inlets, outlets }) =
    let
        inletsStr = String.fromCodePointArray
            $ Array.replicate (Seq.length inlets)
            $ codePointFromChar 'i'
        outletsStr = String.fromCodePointArray
            $ Array.replicate (Seq.length outlets)
            $ codePointFromChar 'o'
        nodeViewStr = "[" <> inletsStr <> "]" <> name <> "[" <> outletsStr <> "]"
    in ML.empty' (String.length nodeViewStr /\ 1 )
        # ML.place (0 /\ 0) nodeViewStr


packNode :: forall d c n. R.Network d c n -> R.Node d n -> Item
packNode nw (R.Node uuid path@(R.ToNode { node : name }) _ _ { inlets, outlets }) =
    R2.item width 1
        { subject : NodeSubj path
        , packing : Nothing
        }
    where
        width = String.length name + Seq.length inlets + Seq.length outlets + 4


viewPatch :: forall d c n. R.Network d c n -> Bounds -> R.Patch d c n -> View
viewPatch nw bounds (R.Patch _ _ { nodes })  =
    let
        patchView = ML.empty' initialBounds
        applyNodeView nodePath curPatchView =
            fromMaybe curPatchView $
                Lens.view (R._node nodePath) nw
                    >>= viewNode nw
                        >>> ML.inject (0 /\ 0) curPatchView
                        >>> pure
    in
        foldr applyNodeView patchView $ Array.fromFoldable nodes


packPatch
    :: forall d c n
     . Bounds
    -> R.Network d c n
    -> R.Patch d c n
    -> Item
packPatch (width /\ height) nw patch@(R.Patch _ (R.ToPatch name) { nodes }) =
    let
        container = R2.container width height
        packing =
            nodes
                # Seq.toUnfoldable
                # map (\path -> Lens.view (R._node path) nw)
                # Array.catMaybes
                # map (packNode nw)
                # List.fromFoldable
                # R2.pack container
                # fromMaybe container
                # Packing
    in
        R2.item width height
            { subject : PatchSubj $ R.ToPatch name
            , packing : Just packing
            }


viewNetwork :: forall d c n. Packing -> R.Network d c n -> View
viewNetwork (Packing b2) nw@(R.Network { name, patches })  =
    R2.unfold foldingF startView b2
    where
        startView = ML.empty' (100 /\ 100)
        foldingF (item /\ (x /\ y /\ w /\ h)) v =
            withSubjectView # withSubPacking
            where
                injectSubjView sView = ML.inject (x /\ y) sView v
                injectSubPacking v' (Packing b2') =
                    R2.unfold foldingF v' b2'
                withSubjectView =
                    viewSubject item.subject w h
                        # maybe v injectSubjView
                withSubPacking v' =
                    maybe v' (injectSubPacking v') item.packing
        viewSubject (PatchSubj patchPath) w h =
            Lens.view (R._patchByPath patchPath) nw >>=
                pure <<< viewPatch nw (w /\ h)
        viewSubject (NodeSubj nodePath) w h =
            Lens.view (R._nodeByPath nodePath) nw >>=
                pure <<< viewNode nw
        viewSubject _ _ _ = Nothing



packNetwork :: forall d c n. R.Network d c n -> Packing -> Packing
packNetwork nw@(R.Network { name, patches }) (Packing container) =
    let
        width /\ height = R2.size container
        patchCount = toNumber $ Seq.length patches
        columns = ceil $ sqrt patchCount
        rows = round $ patchCount / columns
        orphans = round $ patchCount % columns
        patchWidth = round $ toNumber width / columns
        patchHeight = round $ toNumber height
            / toNumber (if (orphans == 0) then rows else 1 + rows)
        actualPatches = Lens.view R._networkPatches nw
    in
        actualPatches
            # map (packPatch (patchWidth /\ patchHeight) nw)
            # R2.pack container
            # fromMaybe container
            # Packing


update
    :: forall d c n
     . Either Msg (C.Action d c n)
    -> Ui /\ R.Network d c n
    -> Ui /\ Array Unit
-- update R.Bang (ui /\ nw) =
--     ui { packing = Just $ ui.packing # packNetwork nw }
update _ (ui /\ _) =
    ui /\ []


view
    :: forall d c n
     . Array R.RpdError /\ Ui /\ R.Network d c n
    -> View
view (errors /\ ui /\ nw) =
    -- "{" <> toString (viewPacking ui.packing) <> toString (viewStatus ui.status) <> "}"
    -- "{" <> show packing <> " :: "
    --     <> show (viewNetwork packing nw) <> " :: "
    --     <> show (viewStatus ui.status) <> "}"
    viewNetwork packing nw
        # ML.inject (0 /\ 0) (viewErrors errors)
    where
        viewErrors [] = ML.from' ""
        viewErrors _ = ML.from' $ "<ERR: " <> joinWith "/" (show <$> errors) <> ">"
        -- FIXME: store the Maybe-Packing in UI and update it only on changes,
        --        and if
        bounds@(width /\ height) = initialBounds
        packing = R2.container width height # Packing # packNetwork nw


instance showPacking :: Show Packing where
    show (Packing r2) = show r2
