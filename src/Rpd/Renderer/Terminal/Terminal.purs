module Rpd.Renderer.Terminal
    ( TerminalRenderer
    , terminalRenderer
    , Ui
    , Packing -- TODO: do not expose maybe?
    , Status -- TODO: do not expose maybe?
    , view -- TODO: do not expose maybe?
    ) where

import Prelude

import Math (ceil, sqrt, (%))
import Data.Int (toNumber, floor, round)
import Data.Array as Array
import Data.Array ((!!))
import Data.String (CodePoint, fromCodePointArray, toCodePointArray, codePointFromChar)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List as List
import Data.List (List, (:))
import Data.Set as Set
import Data.String as String
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldr, foldMap)
import Data.Either (Either(..))
import Data.Lens as Lens

import Data.BinPack.R2 as R2
import Control.Alt ((<|>))

import Rpd.Network (Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)) as R
import Rpd.API (RpdError) as R
import Rpd.Path (Path(..), InletPath, OutletPath, NodePath, PatchId) as R
import Rpd.Optics (_patchNodes, _node) as R
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
    }


type Bounds = { width :: Int, height :: Int }
type Pos = { x :: Int, y :: Int }


initialBounds :: Bounds
initialBounds = { width: 20, height : 20 }


initUi :: Ui
initUi =
    -- { packing : Packing $ R2.container 200 200
    { status : Empty
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
    fromCodePointArray $ Array.concat view


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


inject :: Pos -> View -> View -> View
inject pos source into =
    source -- TODO


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
    let
        width = String.length name + Set.size inlets + Set.size outlets + 4
        -- inletsStr = String.fromCodePointArray
        --     $ Array.replicate (Set.size inlets)
        --     $ codePointFromChar 'i'
        -- outletsStr = String.fromCodePointArray
        --     $ Array.replicate (Set.size outlets)
        --     $ codePointFromChar 'o'
        -- nodeViewStr = "[" <> inletsStr <> "]" <> name <> "[" <> outletsStr <> "]"
        -- nodeView = emptyView { width : width, height : 1 }
        --     # place { x: 0, y: 0 } nodeViewStr
    in
        R2.item width 1
            { subject : R.ToNode path
            , packing : Nothing
            }


viewPatch :: forall d. R.Network d -> Bounds -> View
viewPatch nw bounds =
    emptyView initialBounds


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


viewNetwork :: forall d. R.Network d -> Bounds -> View
viewNetwork nw bounds =
    emptyView bounds
        # place { x: 0, y: 0 } "AA"


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
        <> toString (viewNetwork nw bounds) <> " :: "
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
