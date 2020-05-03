module Xodus.Toolkit.Node where

import Prelude

import Data.Tuple.Nested ((/\))

import Noodle.Process (ProcessF(..)) as R
import Noodle.Toolkit (NodeDef(..), noInlets, withInlets, withOutlets) as T
import Noodle.Toolkit ((~<), (>~))
import Noodle.Render.Atom (class Atom) as R

import Xodus.Toolkit.Value (Value)
import Xodus.Toolkit.Channel (Channel(..))


type NodeDef = T.NodeDef Value Channel


data Node
    = ConnectNode
    | NodeListNode
    | DatabasesNode


instance showNode :: Show Node where
    show NodeListNode = "node list"
    show ConnectNode = "connect"
    show DatabasesNode = "databases"


nodesForTheList :: Array Node
nodesForTheList =
    [ ConnectNode
    , DatabasesNode
    ]


connectNode :: NodeDef
connectNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "bang" /\ Channel
        , outlets :
            T.withOutlets
            >~ "databases" /\ Channel
        , process : R.Process pure  -- FIXME: use `PassThrough`
        }


databaseNode :: NodeDef
databaseNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "databases" /\ Channel
        , outlets :
            T.withOutlets
            >~ "database" /\ Channel
        , process : R.Process pure  -- FIXME: use `PassThrough`
        }


instance nodeAtom :: R.Atom Node where
    labelOf = show
    uniqueIdOf = show
    debugInfoOf = show
