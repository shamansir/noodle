module Rpd.Renderer.Terminal
    ( TerminalRenderer
    , terminalRenderer
    , Ui
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
import Data.Set as Set
import Data.String (CodePoint, fromCodePointArray, toCodePointArray, codePointFromChar)
import Data.String as String
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))

import Data.Either (Either(..))

import Data.Foldable (foldMap, foldr, sequence_)
import Data.Lens as Lens
import Data.Traversable (traverse)

import Data.BinPack.R2 as R2

import Math (ceil, sqrt, (%))

import Rpd.API (RpdError) as R
import Rpd.Network (Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)) as R
import Rpd.Optics (_patchNodes, _node, _patch) as R
import Rpd.Path (Path(..), InletPath, OutletPath, NodePath, PatchId) as R
import Rpd.Render (PushMsg, Message(..)) as R
import Rpd.RenderMUV (Renderer(..))



infixl 8 Array.insertAt as >>


type View = Array (Array CodePoint)

-- data Cell =
--     Cell R.Path View

-- R2.Bin2 Stores information about width/height and x/y
data Packing = Packing (R2.Bin2 Int { subject :: R.Path, packing :: Maybe Packing })

type Item = R2.Item Int { subject :: R.Path, packing :: Maybe Packing }


data Status
    = Empty
    | Error (R.RpdError)
    -- TODO : Selection


type Ui =
    -- { packing : Packing
    { status :: Status
    , invalidate :: Boolean
    }


type Bounds = { width :: Int, height :: Int }
type Pos = { x :: Int, y :: Int }


initialBounds :: Bounds
initialBounds = { width: 20, height : 20 }


initUi :: Ui
initUi =
    -- { packing : Packing $ R2.container 200 200
    { status : Empty
    , invalidate : true
    }


type TerminalRenderer d = Renderer d Ui String


terminalRenderer :: forall d. TerminalRenderer d
terminalRenderer =
    Renderer
        { from : ""
        , init : initUi
        , update
        , view
        }


noView :: View
noView = [[]]


emptyView :: Bounds -> View
emptyView { width, height } =
    Array.replicate height
        $ Array.replicate width spaceCP
    where
        spaceCP = codePointFromChar ' '


viewFrom :: String -> View
viewFrom str =
    Array.singleton $ toCodePointArray str


toString :: View -> String -- a.k.a. Show
toString view =
    -- foldMap fromCodePointArray view
    String.joinWith "\n" (map fromCodePointArray view)


place :: Pos -> String -> View -> View
place pos string view =
    fromMaybe view $ do
        row <- view !! pos.y
        let row' = Array.mapWithIndex mapF row
        view # pos.y >> row'
    where
        strLen = String.length string
        strCP = String.toCodePointArray string
        startAt = pos.x
        mapF index cpoint
            | index < startAt = cpoint
            | index > (startAt + strLen) = cpoint
            | otherwise = fromMaybe cpoint $ strCP !! (index - startAt)


place' :: Pos -> Char -> View -> View
place' pos char view =
    fromMaybe view $ do
        row <- view !! pos.y
        row' <- row # pos.x >> codePointFromChar char
        view # pos.y >> row'


inject :: Pos -> Bounds -> View -> View -> View
inject pos bounds what into =
    Array.mapWithIndex mapRow into
    where
        startCol = pos.x
        startRow = pos.y
        width = bounds.width
        height = bounds.height
        mapRow rowIdx row = Array.mapWithIndex (mapCol rowIdx) row
        mapCol rowIdx colIdx cpoint | rowIdx < startRow || colIdx < startCol = cpoint
        mapCol rowIdx colIdx cpoint | rowIdx >= (startRow + height)
                                      || colIdx >= (startCol + width) = cpoint
        mapCol rowIdx colIdx cpoint | otherwise =
            fromMaybe cpoint $ do
                whatRow <- what !! (rowIdx - startRow)
                whatCp <- whatRow !! (colIdx - startCol)
                pure whatCp


-- codePointAt :: Int -> Int -> View

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
viewStatus _ = viewFrom ">"


packInlet :: forall d. R.Network d -> R.Inlet d -> Item
packInlet nw (R.Inlet path _ _) =
    R2.item 0 0 { subject : R.ToInlet path, packing : Nothing }


packOutlet :: forall d. R.Network d -> R.Outlet d -> Item
packOutlet nw (R.Outlet path _ _) =
    R2.item 0 0 { subject : R.ToOutlet path, packing : Nothing }


viewNode :: forall d. R.Network d -> R.Node d -> View
viewNode nw (R.Node path { name } { inlets, outlets }) =
    let
        inletsStr = String.fromCodePointArray
            $ Array.replicate (Set.size inlets)
            $ codePointFromChar 'i'
        outletsStr = String.fromCodePointArray
            $ Array.replicate (Set.size outlets)
            $ codePointFromChar 'o'
        nodeViewStr = "[" <> inletsStr <> "]" <> name <> "[" <> outletsStr <> "]"
    in emptyView { width : String.length nodeViewStr, height : 1 }
        # place { x: 0, y: 0 } nodeViewStr


packNode :: forall d. R.Network d -> R.Node d -> Item
packNode nw (R.Node path { name } { inlets, outlets }) =
    R2.item width 1
        { subject : R.ToNode path
        , packing : Nothing
        }
    where
        width = String.length name + Set.size inlets + Set.size outlets + 4


viewPatch :: forall d. R.Network d -> Bounds -> R.Patch d -> View
viewPatch nw bounds (R.Patch _ _ { nodes })  =
    let
        patchView = emptyView initialBounds
        applyNodeView nodePath curPatchView =
            fromMaybe curPatchView $
                Lens.view (R._node nodePath) nw
                    >>= viewNode nw
                        >>> inject { x: 0, y: 0 } { width : 20, height : 20 } curPatchView
                        >>> pure
    in
        foldr applyNodeView patchView $ Array.fromFoldable nodes


packPatch
    :: forall d
     . Bounds
    -> R.Network d
    -> R.Patch d
    -> Item
packPatch { width, height } nw patch@(R.Patch patchId { name } { nodes }) =
    let
        container = R2.container width height
        packing =
            nodes
                # Set.toUnfoldable
                # map (\path -> Lens.view (R._node path) nw)
                # Array.catMaybes
                # map (packNode nw)
                # List.fromFoldable
                # R2.pack container
                # fromMaybe container
                # Packing
    in
        R2.item width height
            { subject : R.ToPatch patchId
            , packing : Just packing
            }


viewNetwork :: forall d. Packing -> R.Network d -> View
viewNetwork (Packing b2) nw@(R.Network { name } { patches })  =
    R2.unfold foldingF startView b2
    where
        startView = emptyView { width: 200, height : 200 }
        foldingF (item /\ (x /\ y /\ w /\ h)) v =
            withSubjectView # withSubPacking
            where
                injectSubjView sView = inject { x, y } { width: w, height : h } sView v
                injectSubPacking v' (Packing b2') =
                    R2.unfold foldingF v' b2'
                withSubjectView =
                    viewSubject item.subject w h
                        # maybe v injectSubjView
                withSubPacking v' =
                    maybe v' (injectSubPacking v') item.packing
        viewSubject (R.ToPatch patchId) w h =
            Lens.view (R._patch patchId) nw >>=
                pure <<< viewPatch nw { width: w, height : h }
        viewSubject (R.ToNode nodePath) w h =
            Lens.view (R._node nodePath) nw >>=
                pure <<< viewNode nw
        viewSubject _ _ _ = Nothing



packNetwork :: forall d. R.Network d -> Packing -> Packing
packNetwork nw@(R.Network { name } { patches }) (Packing container) =
    let
        width /\ height = R2.size container
        patchCount = toNumber $ Map.size patches
        columns = ceil $ sqrt patchCount
        rows = round $ patchCount / columns
        orphans = round $ patchCount % columns
        patchWidth = round $ toNumber width / columns
        patchHeight = round $ toNumber height
            / toNumber (if (orphans == 0) then rows else 1 + rows)
    in
        Map.values patches
            # map (packPatch { width : patchWidth, height : patchHeight } nw)
            # R2.pack container
            # fromMaybe container
            # Packing


update :: forall d. R.Message d -> (Ui /\ R.Network d) -> Ui
-- update R.Bang (ui /\ nw) =
--     ui { packing = Just $ ui.packing # packNetwork nw }
update _ (ui /\ _) =
    ui


view :: forall d. R.PushMsg d -> Either R.RpdError (Ui /\ R.Network d) -> String
view pushMsg (Right (ui /\ nw)) =
    -- "{" <> toString (viewPacking ui.packing) <> toString (viewStatus ui.status) <> "}"
    "{" <> show packing <> " :: "
        <> toString (viewNetwork packing nw) <> " :: "
        <> toString (viewStatus ui.status) <> "}"
    where
        -- FIXME: store the Maybe-Packing in UI and update it only on changes,
        --        and if
        bounds@{ width, height } = initialBounds
        packing = R2.container width height # Packing # packNetwork nw
view pushMsg (Left err) =
    "ERR: " <> show err


instance showPacking :: Show Packing where
    show (Packing r2) = show r2
