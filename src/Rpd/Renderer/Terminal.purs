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
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldr, foldMap)
import Data.Either (Either(..))

import Rpd.Network (Network(..), Patch(..), Node(..), Inlet(..), Outlet(..), Link(..)) as R
import Rpd.API (RpdError) as R
import Rpd.Path (Path(..)) as R
import Rpd.Render (PushMsg, Message(..)) as R
import Rpd.RenderS (Renderer(..))


data Block =
    Block
        { target :: R.Path
        , x :: Int
        , y :: Int
        , width :: Int
        , height :: Int
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


blocksOfInlet :: forall d. R.Inlet d -> List Block
blocksOfInlet inlet = List.Nil


blocksOfOutlet :: forall d. R.Outlet d -> List Block
blocksOfOutlet outlet = List.Nil


blocksOfNode :: forall d. R.Node d -> List Block
blocksOfNode node = List.Nil


blocksOfPatch :: forall d. R.Patch d -> List Block
blocksOfPatch patch = List.Nil


blocksOfNetwork :: forall d. ( Int /\ Int ) -> R.Network d -> List Block
blocksOfNetwork (x /\ y) (R.Network { name } { patches }) =
    List.singleton $
        Block
            { target : R.ToNetwork
            , x
            , y
            , width : String.length content
            , height : 1
            , content
            }
            $ foldMap blocksOfPatch patches
    where
        content = "[" <> name <> "]"


update :: forall d. R.Message d -> Ui -> R.Network d -> Ui
update R.Bang _ nw =
    { blocks : blocksOfNetwork (0 /\ 0) nw, status: Empty }
update _ ui _ =
    ui


view :: forall d. R.PushMsg d -> Either R.RpdError (Ui /\ R.Network d) -> String
view pushMsg (Right (ui /\ _)) =
    foldBlocks ui.blocks <> viewStatus ui.status
view pushMsg (Left err) =
    "ERR: " <> show err
