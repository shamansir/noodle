module Rpd.Render.Terminal
    ( TerminalRenderer
    , terminalRenderer
    , view -- TODO: do not expose maybe?
    ) where

import Prelude

import Data.Map as Map
import Data.List as List
import Data.Set as Set
import Data.Either (Either(..))
import Data.String (joinWith)

import Rpd.Network (Network(..), Patch(..)) as R
import Rpd.API (RpdError) as R
import Rpd.Render (PushMsg, Renderer(..))


type TerminalRenderer d = Renderer d String


terminalRenderer :: forall d. TerminalRenderer d
terminalRenderer =
    Renderer "" view


view :: forall d. PushMsg d -> Either R.RpdError (R.Network d)  -> String
view pushMsg (Right (R.Network _ { patches })) =
    "SUCC" <> patchesInfo
    where
        patchesInfo = joinWith "," $ (getNodesCount <$> Map.values patches) # List.toUnfoldable
        getNodesCount (R.Patch _ _ { nodes }) =
            show $ Set.size nodes
view pushMsg (Left err) =
    "ERR: " <> show err
