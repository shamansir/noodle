module Noodle.Render.Terminal
    ( TerminalRenderer
    , make
    , Ui
    , View
    , Msg
    , Packing -- TODO: do not expose maybe?
    , Status -- TODO: do not expose maybe?
    , init
    , view -- TODO: do not expose maybe?
    ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber, round)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Sequence as Seq
import Data.String (codePointFromChar, joinWith)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Data.Foldable (foldr)
import Data.Lens as Lens

import Data.BinPack.R2 as R2

import Math (ceil, sqrt, (%))

import Data.Covered

import FSM (doNothing)

import Noodle.API.Errors (NoodleError) as R
import Noodle.API.Action as C
import Noodle.Network (Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)) as R
import Noodle.Optics as R
import Noodle.Path as R
import Noodle.Render.Renderer (Renderer, Routed) as R
import Noodle.Render.Renderer (make) as Renderer
import Noodle.Toolkit as T


import Noodle.Render.Terminal.Multiline as ML


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
    | Error (R.NoodleError)
    | WritingCommand String
    -- TODO : Selection


type Ui =
    -- { packing : Packing
    { status :: Status
    , invalidate :: Boolean
    , lastErrors :: Array R.NoodleError
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


init :: Ui
init =
    -- { packing : Packing $ R2.container 200 200
    { status : Empty
    , invalidate : true
    , lastErrors : []
    }


type TerminalRenderer d c n = R.Renderer d c n Msg Ui View


make :: forall d c n. T.Toolkit d c n -> TerminalRenderer d c n
make toolkit =
    Renderer.make
        toolkit
        (const update)
        (recover >>> view)


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
     . R.Routed Msg (C.Action d c n)
    -> Covered R.NoodleError (Ui /\ R.Network d c n)
    -> Covered R.NoodleError (Ui /\ R.Network d c n)
        /\ List (Effect (R.Routed Msg (C.Action d c n)))
-- update R.Bang (ui /\ nw) =
--     ui { packing = Just $ ui.packing # packNetwork nw }
update _ covered =
    let (ui /\ nw) = recover covered
    in (carry $ (covered # withError addError ui) /\ nw)
        /\ doNothing


addError :: R.NoodleError -> Ui  -> Ui
addError error ui =
    ui { lastErrors = ui.lastErrors <> [ error ]}


view
    :: forall d c n
     . Ui /\ R.Network d c n
    -> View
view (ui /\ nw) =
    -- "{" <> toString (viewPacking ui.packing) <> toString (viewStatus ui.status) <> "}"
    -- "{" <> show packing <> " :: "
    --     <> show (viewNetwork packing nw) <> " :: "
    --     <> show (viewStatus ui.status) <> "}"
    viewNetwork packing nw
        # ML.inject (0 /\ 0) (viewErrors ui.lastErrors)
    where
        viewErrors :: Array R.NoodleError -> View
        viewErrors [] = ML.from' ""
        viewErrors errors = ML.from' $ "<ERR: " <> joinWith "/" (show <$> errors) <> ">"
        -- FIXME: store the Maybe-Packing in UI and update it only on changes,
        --        and if
        bounds@(width /\ height) = initialBounds
        packing = R2.container width height # Packing # packNetwork nw


instance showPacking :: Show Packing where
    show (Packing r2) = show r2
