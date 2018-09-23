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

import Data.Array as Array
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
import Rpd.Path (Path(..)) as R
import Rpd.Optics (_patchNodes) as R
import Rpd.Render (PushMsg, Message(..)) as R
import Rpd.RenderS (Renderer(..))


type View = Array (Array CodePoint)

data Cell =
    Cell R.Path View

-- R2.Bin2 Stores information about width/height and x/y
data Packing = Packing (R2.Bin2 Int { cell :: Cell, packing :: Maybe Packing })

--type PackItem = R2.Item Int { cell :: Cell, packing :: Maybe Packing }


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


toString :: View -> String
toString view =
    "" -- TODO


place :: { x :: Int, y :: Int } -> String -> View -> View
place pos string view =
    view -- TODO


place' :: { x :: Int, y :: Int } -> Char -> View -> View
place' pos char view =
    view -- TODO


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


packInlet :: forall d. R.Network d -> R.Inlet d -> Packing
packInlet nw inlet = Packing $ R2.container 0 0


packOutlet :: forall d. R.Network d -> R.Outlet d -> Packing
packOutlet nw outlet = Packing $ R2.container 0 0


packNode :: forall d. R.Network d -> R.Node d -> Packing
packNode nw node = Packing $ R2.container 0 0


packPatch :: forall d. R.Network d -> R.Patch d -> Packing -> Packing
packPatch nw patch@(R.Patch patchId { name } { nodes }) root =
    Packing $ R2.container 0 0
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
        container = Packing $ R2.container width height
    in
        foldr (packPatch nw) container $ Map.values patches

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
