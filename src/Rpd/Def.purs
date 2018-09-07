module Rpd.Def
    -- TODO: remove `-Def` from inner names
    ( PatchDef(..), NodeDef(..), InletDef(..), OutletDef(..)
    , ProcessF(..)
    )
    where

import Data.List (List)
import Data.Maybe (Maybe)

import Rpd.Util (type (/->))

-- TODO: may be find better ways to process these things in future
--       I'd like to have something similar to JS-world
--       function (inlets) { return { 'c': inlets.a + inlets.b } }
-- variants:
--  `Record.set` / `Record.get` etc.
--  `Foreign.Object`` : https://github.com/purescript/purescript-foreign-object/blob/master/src/Foreign/Object.purs
--  `liftA2 (+) (m^.at a) (m^.at b)` -- Map -> Map

-- may be ProcessF should also receive previous value
-- TODO: add Process Behavior (a.k.a. event with function) it would be possible
--       to subscribe/know outlets changes as well
data ProcessF d
    = FlowThrough
    | IndexBased (Array d -> Array d)
    | LabelBased ((String /-> d) -> (String /-> d))


-- data DataSource d
--     = UserSource (Flow d)
--     | OutletSource OutletPath (Flow d)

type PatchDef d =
    { name :: String
    , nodeDefs :: List (NodeDef d)
    }
type NodeDef d =
    { name :: String
    , inletDefs :: List (InletDef d)
    , outletDefs :: List (OutletDef d)
    , process :: ProcessF d
    }
type InletDef d =
    { label :: String
    , default :: Maybe d
    , accept :: Maybe (d -> Boolean)
    --, adapt ::
    }
type OutletDef d =
    { label :: String
    }
