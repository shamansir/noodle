module Front.Shared.DocumentationFocus where

import Prelude

import Data.Maybe (Maybe)

import Noodle.Id as Id
import Noodle.Raw.Node (NodeChanges) as Raw


type DocumentationFocus sr cr =
    { node :: Id.NodeR
    , curUpdate :: Maybe (Raw.NodeChanges sr cr)
    }