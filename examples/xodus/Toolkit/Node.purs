module Xodus.Toolkit.Node where

import Prelude

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.List ((:), List(..))

import Noodle.Process (ProcessF(..)) as R
import Noodle.Toolkit (NodeDef(..), noInlets, withInlets, withOutlets) as T
import Noodle.Toolkit ((~<), (>~))
import Noodle.Render.Atom (class Atom) as R

import Xodus.Toolkit.Value (Value(..), QueryResult(..))
import Xodus.Toolkit.Channel (Channel(..))
import Xodus.Toolkit.Dto
import Xodus.Toolkit.Requests


type NodeDef = T.NodeDef Value Channel


data Node
    = ConnectNode
    | NodeListNode
    | DatabasesNode
    | QueryNode


instance showNode :: Show Node where
    show NodeListNode = "node list"
    show ConnectNode = "connect"
    show DatabasesNode = "databases"
    show QueryNode = "query"


nodesForTheList :: Array Node
nodesForTheList =
    [ ConnectNode
    , DatabasesNode
    , QueryNode
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
       , process : R.ProcessAff
            $ \receive -> do
                databases <- getDatabases
                let
                    send "databases" = Just $ Databases databases
                    send _ = Nothing
                pure send
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
        , process : R.Withhold
        }


queryNode :: NodeDef
queryNode =
    T.NodeDef
        { inlets :
            T.withInlets
            ~< "database" /\ Channel
        , outlets :
            T.withOutlets
            >~ "enitities" /\ Channel
            >~ "types" /\ Channel
        , process : R.ProcessAff
            $ \receive ->
                case receive "database" of
                    Just (Source database) -> do
                        entityTypes <- getEntityTypes database
                        let
                            send "types" = Just $ Result $ HasEntityTypes entityTypes
                            send _ = Nothing
                        pure send
                    _ -> pure $ const Nothing
        }


instance nodeAtom :: R.Atom Node where
    labelOf = show
    uniqueIdOf = show
    debugInfoOf = show
