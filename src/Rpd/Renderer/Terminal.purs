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
import Data.List (List, (:))
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
type Size = { width :: Int, height :: Int }
type Rect = Coord /\ Size


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


emptyBlock :: Block
emptyBlock =
    Block
        { target : R.Unknown
        , rect : zeroRect
        , content : ""
        }
        List.Nil


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


blockOfInlet :: forall d. R.Network d -> R.Inlet d -> Block
blockOfInlet nw inlet = emptyBlock


blockOfOutlet :: forall d. R.Network d -> R.Outlet d -> Block
blockOfOutlet nw outlet = emptyBlock


blockOfNode :: forall d. R.Network d -> R.Node d -> Block
blockOfNode nw node = emptyBlock


blockOfPatch :: forall d. Rect -> Coord -> R.Network d -> R.Patch d -> Block
blockOfPatch bounds origin nw patch@(R.Patch patchId { name } { nodes }) =
    Block
        { target : R.ToPatch patchId
        , rect
        , content
        }
        $ Tuple.snd
        $ foldr
            foldingF
            (origin /\ List.Nil)
            nodes
    where
        content = "[" <> name <> "]"
        size = { width: String.length content, height: 1 }
        rect = getNextRect bounds origin size
        (nodes :: (List (R.Node d))) = Lens.view (R._patchNodes patchId) nw
        foldingF node (prevOrigin /\ blocks) =
            (/\) nextOrigin $ nextBlock : blocks
                where
                    nextBlock = blockOfNode nw node
                    (nextOrigin :: Coord) = getOriginFrom nextBlock



blockOfNetwork :: forall d. Rect -> Coord -> R.Network d -> Block
blockOfNetwork bounds origin nw@(R.Network { name } { patches }) =
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
        size = { width: String.length content, height: 1 }
        rect = getNextRect bounds origin size
        foldingF patch (prevOrigin /\ blocks) =
            (/\) nextOrigin $ nextBlock : blocks
                where
                    nextBlock = blockOfPatch bounds prevOrigin nw patch
                    nextOrigin = getOriginFrom nextBlock


coord :: Int -> Int -> Coord
coord x y = { x, y }


size :: Int -> Int -> Size
size width height = { width, height }


zero :: Coord
zero = coord 0 0


zeroRect :: Rect
zeroRect = coord 0 0 /\ size 0 0


move :: Coord -> Coord -> Coord
move { x: fromX, y: fromY } { x: shiftX, y: shiftY } =
    coord (fromX + shiftX) (fromY + shiftY)


getNextRect :: Rect -> Coord -> Size -> Rect
getNextRect (_ /\ { width: bwidth }) { x, y } { width, height } =
    if ((x + width) >= bwidth) then
        coord 0 (y + height) /\ size width height
    else
        coord (x + width) y /\ size width height


getOriginFrom :: Block -> Coord
getOriginFrom (Block { rect } _) =
    let ({ x, y } /\ { width, height }) = rect
    in coord (x + width) (y + height)


update :: forall d. R.Message d -> Ui -> R.Network d -> Ui
update R.Bang _ nw =
    { blocks : List.singleton
        $ blockOfNetwork (zero /\ size 200 200) zero nw
    , status: Empty
    }
update _ ui _ =
    ui


view :: forall d. R.PushMsg d -> Either R.RpdError (Ui /\ R.Network d) -> String
view pushMsg (Right (ui /\ _)) =
    foldBlocks ui.blocks <> viewStatus ui.status
view pushMsg (Left err) =
    "ERR: " <> show err
