module Rpd.Renderer.Terminal
    ( TerminalRenderer
    , terminalRenderer
    , Ui
    , Block -- TODO: do not expose maybe?
    , Status -- TODO: do not expose maybe?
    , view -- TODO: do not expose maybe?
    ) where

import Prelude

import Data.Map as Map
import Data.List as List
import Data.List (List)
import Data.Set as Set
import Data.String as String
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldr, foldMap)
import Data.Either (Either(..))
import Data.Lens as Lens

import Rpd.Network (Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)) as R
import Rpd.API (RpdError) as R
import Rpd.Path (Path(..)) as R
import Rpd.Optics (_patchNodes) as R
import Rpd.Render (PushMsg, Message(..)) as R
import Rpd.RenderS (Renderer(..))


type Coord = { x :: Int, y :: Int }
type Rect = Coord /\ Coord


data Block =
    Block
        { target :: R.Path
        , rect :: Rect
        , content :: String
        }
        (List Block)


data Status
    = Empty
    | Error (R.RpdError)
    -- TODO : Selection


type Ui =
    { blocks :: List Block
    , status :: Status
    }


initUi :: Ui
initUi =
    { blocks : List.Nil
    , status : Empty
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


-- TODO: implement Monoid + Foldable + ...
foldBlocks :: List Block -> String
foldBlocks blocks =
    foldr foldBlock "" blocks
    where
        foldBlock (Block { content } blocks') prev =
            prev <> content <> foldBlocks blocks'


viewStatus :: Status -> String
viewStatus _ = "> "


blocksOfInlet :: forall d. R.Network d -> R.Inlet d -> List Block
blocksOfInlet nw inlet = List.Nil


blocksOfOutlet :: forall d. R.Network d -> R.Outlet d -> List Block
blocksOfOutlet nw outlet = List.Nil


blocksOfNode :: forall d. R.Network d -> R.Node d -> List Block
blocksOfNode nw node = List.Nil


blocksOfPatch :: forall d. Coord -> R.Network d -> R.Patch d -> List Block
blocksOfPatch origin nw patch@(R.Patch patchId { name } { nodes }) =
    List.singleton $
        Block
            { target : R.ToPatch patchId
            , rect
            , content
            }
            $ foldMap (blocksOfNode nw) (Lens.view (R._patchNodes patchId) nw)
            -- $ foldr (\_ _ -> List.Nil) List.Nil Lens.view (R._patchNodes patchId) nw
    where
        shift = { x: String.length content, y: 1 }
        rect = origin /\ move origin shift
        content = "[" <> name <> "]"


blocksOfNetwork :: forall d. Rect -> Coord -> R.Network d -> List Block
blocksOfNetwork bounds origin nw@(R.Network { name } { patches }) =
    List.singleton $
        Block
            { target : R.ToNetwork
            , rect
            , content
            }
            $ Tuple.snd
            $ foldr
                foldingF
                (origin /\ List.Nil)
                patches
    where
        content = "[" <> name <> "]"
        shift = { x: String.length content, y: 1 }
        rect = origin /\ move origin shift
        foldingF patch (prevOrigin /\ blocks) =
            (/\) prevOrigin $ blocks <> blocksOfPatch prevOrigin nw patch


coord :: Int -> Int -> Coord
coord x y = { x: 0, y: 0 }


zero :: Coord
zero = coord 0 0


move :: Coord -> Coord -> Coord
move { x: fromX, y: fromY } { x: shiftX, y: shiftY } =
    coord (fromX + shiftX) (fromY + shiftY)


update :: forall d. R.Message d -> Ui -> R.Network d -> Ui
update R.Bang _ nw =
    { blocks : blocksOfNetwork (zero /\ coord 200 200) zero nw, status: Empty }
update _ ui _ =
    ui


view :: forall d. R.PushMsg d -> Either R.RpdError (Ui /\ R.Network d) -> String
view pushMsg (Right (ui /\ _)) =
    foldBlocks ui.blocks <> viewStatus ui.status
view pushMsg (Left err) =
    "ERR: " <> show err
