module Xodus.Toolkit.Node where

import Prelude

import Data.Tuple.Nested ((/\))

import Noodle.Process (ProcessF(..)) as R
import Noodle.Toolkit as T
import Noodle.Toolkit (withInlets, withOutlets, (~<), (>~))
import Noodle.Render.Atom (class Atom) as R -- FIXME: shouldn't require `Render` module

import Xodus.Toolkit.Value (Value)
import Xodus.Toolkit.Channel (Channel(..))


type NodeDef = T.NodeDef Value Channel


data Node
    = BangNode
    | NodeListNode


instance showNode :: Show Node where
    show NodeListNode = "node list"
    show BangNode = "bang"


nodesForTheList :: Array Node
nodesForTheList =
    [ BangNode
    ]


bangNode :: NodeDef
bangNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "bang" /\ Channel
        , outlets :
            T.withOutlets
            >~ "bang" /\ Channel
        , process : R.Process pure  -- FIXME: use `PassThrough`
        }


instance nodeAtom :: R.Atom Node where
    labelOf = show
    uniqueIdOf = show
    debugInfoOf = show
