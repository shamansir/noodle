module Rpd.Renderer.Terminal
    ( TerminalRenderer
    , terminalRenderer
    , Ui
    , Cell -- TODO: do not expose maybe?
    , Packing -- TODO: do not expose maybe?
    , Status -- TODO: do not expose maybe?
    , view -- TODO: do not expose maybe?
    ) where

import Prelude

import Math (ceil, sqrt, (%))
import Data.Int (toNumber, floor, round)
import Data.Array as Array
import Data.Array ((!!))
import Data.String (CodePoint, fromCodePointArray, codePointFromChar)
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
import Rpd.RenderS (Renderer(..))



infixl 8 Array.insertAt as >>


type View = Array (Array CodePoint)

data Cell =
    Cell R.Path View

-- R2.Bin2 Stores information about width/height and x/y
data Packing = Packing (R2.Bin2 Int { cell :: Cell, packing :: Maybe Packing })

type Item = R2.Item Int { cell :: Cell, packing :: Maybe Packing }


data Status
    = Empty
    | Error (R.RpdError)
    -- TODO : Selection


type Ui =
    { packing :: Packing
    , status :: Status
    }


initUi :: { width :: Int, height :: Int } -> Ui
initUi { width, height } =
    { packing : Packing $ R2.container width height
    , status : Empty
    }


type TerminalRenderer d = Renderer d Ui String


terminalRenderer :: forall d. TerminalRenderer d
terminalRenderer =
    Renderer
        { from : ""
        , init : initUi { width: 200, height: 200 }
        , update
        , view
        }


noView :: View
noView = [[]]


emptyView :: { width :: Int, height :: Int } -> View
emptyView { width, height } =
    Array.replicate height
        $ Array.replicate width spaceCP
    where
        spaceCP = codePointFromChar ' '


toString :: View -> String
toString view =
    "" -- TODO


place :: { x :: Int, y :: Int } -> String -> View -> View
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


place' :: { x :: Int, y :: Int } -> Char -> View -> View
place' pos char view =
    fromMaybe view $ do
        row <- view !! pos.y
        row' <- row # pos.x >> codePointFromChar char
        view # pos.y >> row'


inject :: { x :: Int, y :: Int } -> View -> View -> View
inject pos source into =
    into -- TODO


viewPacking :: Packing -> View
viewPacking (Packing r2) =
    Array.singleton $ Array.singleton $ codePointFromChar '-'

    -- TODO: make Foldable instance
    -- fromMaybe "[]" $ foldConcat <$> R2.valueOf r2
    -- where
    --     foldCell :: Cell -> View
    --     foldCell (Cell _ content) = foldr (<>) "" $ fromCodePointArray <$> content
    --     foldPacking' :: Maybe Packing -> View
    --     foldPacking' packing = packing >>= pure <<< R2.toList # fromMaybe ""
    --     foldConcat { cell, packing } = foldCell cell <> foldPacking' packing


viewStatus :: Status -> View
viewStatus _ = Array.singleton $ Array.singleton $ codePointFromChar '>'


packInlet :: forall d. R.Network d -> R.Inlet d -> Item
packInlet nw (R.Inlet path _ _) =
    R2.item 0 0 { cell : Cell (R.ToInlet path) noView, packing : Nothing }


packOutlet :: forall d. R.Network d -> R.Outlet d -> Item
packOutlet nw (R.Outlet path _ _) =
    R2.item 0 0 { cell : Cell (R.ToOutlet path) noView, packing : Nothing }


packNode :: forall d. R.Network d -> R.Node d -> Item
packNode nw (R.Node path { name } { inlets, outlets }) =
    let
        width = String.length name + Set.size inlets + Set.size outlets + 4
        inletsStr = String.fromCodePointArray
            $ Array.replicate (Set.size inlets)
            $ codePointFromChar 'i'
        outletsStr = String.fromCodePointArray
            $ Array.replicate (Set.size outlets)
            $ codePointFromChar 'o'
        nodeViewStr = "[" <> inletsStr <> "]" <> name <> "[" <> outletsStr <> "]"
        nodeView = emptyView { width : width, height : 1 }
            # place { x: 0, y: 0 } nodeViewStr
    in
        R2.item width 1
            { cell : Cell (R.ToNode path) nodeView
            , packing : Nothing
            }


packPatch
    :: forall d
     . { width :: Int, height :: Int }
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
        addToView { cell } =
            case cell of
                (Cell _ nodeView) -> inject { x: 0, y: 0 } nodeView
        patchView =
            case packing of
                (Packing p) ->
                    foldr addToView
                        (emptyView { width : width, height : height }) p

            -- # place { x: 0, y: 0 } "TEST"
    in
        R2.item width height
            { cell : Cell (R.ToPatch patchId) patchView
            , packing : Just packing
            }
    -- R2.item 0 0 { cell : Cell (R.ToPatch patchId) noView, packing : Nothing }

    -- where
    --     content = "[" <> name <> "]"
    --     size = { width: String.length content, height: 1 }
    --     (nodes :: (List (R.Node d))) = Lens.view (R._patchNodes patchId) nw
    --     foldingF node (root /\ blocks) =
    --         (/\) root $ nextBlocks : blocks
    --             where
    --                 nextBlocks = blockOfNode nw node


packNetwork :: forall d. { width :: Int, height :: Int } -> R.Network d -> Packing
packNetwork { width, height } nw@(R.Network { name } { patches }) =
    let
        patchCount = toNumber $ Map.size patches
        columns = ceil $ sqrt patchCount
        rows = round $ patchCount / columns
        orphans = round $ patchCount % columns

        container = R2.container width height
        patchWidth = round $ toNumber width / columns
        patchHeight = round $ toNumber height
            / toNumber (if (orphans == 0) then rows else 1 + rows)
    in
        Map.values patches
            # map
                (\patch ->
                    packPatch { width : patchWidth, height : patchHeight } nw patch)
            # R2.pack container
            # fromMaybe container
            # Packing

    -- nwPacking = packNetwork size nw
    -- nwCell = viewPacking nwPacking
    -- packing' = case ui.packing of
    --     Packing r2 ->
    --         R2.pack r2
    --             $ List.singleton
    --             $ R2.item 200 200
    --             $ { cell : Cell nwCell
    --               , packing : Just nwPacking
    --               }


    -- let
    --     patchContainer = R2.container width height
    --     patchContainer' = map ?wh patches
    --     patchesView = viewPacking patchContainer'
    -- in
    --     R2.item 0 0
    --         { cell : Cell R.ToNetwork patchesView
    --         , packing : patchContainer'
    --         }


    -- R2.pack root $
    --     List.singleton $
    --         R2.item width height
    --             { value : Block R.ToNetwork $ List.singleton content
    --             , blocks
    --             }
    -- -- <|> R2.container width height
    -- where
    --     content = "[" <> name <> "]"
    --     root = R2.container width height
    --     blocks = Tuple.snd
    --         $ foldr
    --             foldingF
    --             (origin /\ List.Nil)
    --             root
    --     foldingF patch (root /\ blocks) =
    --         (/\) root $ nextBlocks : blocks
    --             where
    --                 nextBlocks = blocksOfPatch root nw patch


update :: forall d. R.Message d -> Ui -> R.Network d -> Ui
update R.Bang _ nw =
    let
        size = { width : 200, height : 200 }
        ui = initUi size
    in
        ui { packing = packNetwork size nw }

update _ ui _ =
    ui


view :: forall d. R.PushMsg d -> Either R.RpdError (Ui /\ R.Network d) -> String
view pushMsg (Right (ui /\ _)) =
    toString (viewPacking ui.packing) <> toString (viewStatus ui.status)
view pushMsg (Left err) =
    "ERR: " <> show err
