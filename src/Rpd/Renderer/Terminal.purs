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

import Data.String (CodePoint, fromCodePointArray)
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

import Rpd.Network (Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)) as R
import Rpd.API (RpdError) as R
import Rpd.Path (Path(..)) as R
import Rpd.Optics (_patchNodes) as R
import Rpd.Render (PushMsg, Message(..)) as R
import Rpd.RenderS (Renderer(..))


data Cell =
    Cell R.Path (Array (Array CodePoint))


data Packing = Packing (R2.Bin2 Int { cell :: Cell, packing :: Maybe Packing })


data Status
    = Empty
    | Error (R.RpdError)
    -- TODO : Selection


type Ui =
    { packing :: Packing
    , status :: Status
    }


initUi :: { w :: Int, h :: Int } -> Ui
initUi { w, h } =
    { packing : Packing $ R2.container w h
    , status : Empty
    }


type TerminalRenderer d = Renderer d Ui String


terminalRenderer :: forall d. TerminalRenderer d
terminalRenderer =
    Renderer
        { from : ""
        , init : initUi { w: 200, h: 200 }
        , update
        , view
        }


foldPacking :: Packing -> String
foldPacking (Packing r2) =
    -- TODO: make Foldable instance
    fromMaybe "[]" $ foldConcat <$> R2.valueOf r2
    where
        foldCell :: Cell -> String
        foldCell (Cell _ content) = foldr (<>) "" $ fromCodePointArray <$> content
        foldPacking' :: Maybe Packing -> String
        foldPacking' packing = packing >>= pure <<< foldPacking # fromMaybe ""
        foldConcat { cell, packing } = foldCell cell <> foldPacking' packing


viewStatus :: Status -> String
viewStatus _ = "> "


blockOfInlet :: forall d. R.Network d -> R.Inlet d -> Packing
blockOfInlet nw inlet = Packing $ R2.container 0 0


blockOfOutlet :: forall d. R.Network d -> R.Outlet d -> Packing
blockOfOutlet nw outlet = Packing $ R2.container 0 0


blockOfNode :: forall d. R.Network d -> R.Node d -> Packing
blockOfNode nw node = Packing $ R2.container 0 0


blocksOfPatch :: forall d. Packing -> R.Network d -> R.Patch d -> Packing
blocksOfPatch root nw patch@(R.Patch patchId { name } { nodes }) =
    Packing $ R2.container 0 0
    -- where
    --     content = "[" <> name <> "]"
    --     size = { width: String.length content, height: 1 }
    --     (nodes :: (List (R.Node d))) = Lens.view (R._patchNodes patchId) nw
    --     foldingF node (root /\ blocks) =
    --         (/\) root $ nextBlocks : blocks
    --             where
    --                 nextBlocks = blockOfNode nw node


blocksOfNetwork :: forall d. { width :: Int, height :: Int } -> R.Network d -> Packing
blocksOfNetwork { width, height } nw@(R.Network { name } { patches }) =
    Packing $ R2.container 0 0
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
update R.Bang _ _ =
    initUi { w : 200, h :  200 }
update _ ui _ =
    ui


view :: forall d. R.PushMsg d -> Either R.RpdError (Ui /\ R.Network d) -> String
view pushMsg (Right (ui /\ _)) =
    foldPacking ui.packing <> viewStatus ui.status
view pushMsg (Left err) =
    "ERR: " <> show err
