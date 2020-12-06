module Noodle.Render.Context where

import Prelude (negate)

import Data.Map as Map

import Noodle.Util (Position, type (/->))
import Noodle.UUID as UUID
import Noodle.Path as P

-- import Noodle.Render.Toolkit (Renderer)


type Context d c n =
    { mousePos :: Position
    --, renderer :: Renderer d c n view
    , positions :: UUID.Tagged /-> Position
    , lastInletData :: P.ToInlet /-> d
    , lastOutletData :: P.ToOutlet /-> d
    }


init :: forall d c n. Context d c n
init =
    { mousePos : { x : -1.0, y : -1.0 }
    , positions : Map.empty -- FIXME: exclude nodes (they are stored in the `packing`)
    , lastInletData : Map.empty
    , lastOutletData : Map.empty
    }
