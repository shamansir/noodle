module Xodus.Toolkit.Node where

import Prelude

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.List ((:), List(..))
import Data.Foldable (fold)
import Data.Traversable (traverse)

import Noodle.Process (ProcessF(..)) as R
import Noodle.Toolkit (NodeDef(..), noInlets, withInlets, withOutlets) as T
import Noodle.Toolkit ((~<), (>~))
import Noodle.Toolkit (defineNode) as T
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
    T.defineNode
        (T.withInlets
            ~< "bang" /\ Channel)
        (T.withOutlets
            >~ "databases" /\ Channel)
        $ R.ProcessAff
            $ \receive -> do
                databases <- getDatabases
                let
                    send "databases" = Just $ Databases databases
                    send _ = Nothing
                pure send


databaseNode :: NodeDef
databaseNode =
    T.defineNode
        (T.withInlets
            ~< "databases" /\ Channel)
        (T.withOutlets
            >~ "database" /\ Channel)
        R.Withhold


queryNode :: NodeDef
queryNode =
    T.defineNode
        (T.withInlets
            ~< "database" /\ Channel)
        (T.withOutlets
            >~ "entities" /\ Channel
            >~ "types" /\ Channel)
        $ R.ProcessAff
            $ \receive ->
                case receive "database" of
                    Just (Source database) -> do
                        entityTypes <- getEntityTypes database
                        allEntities <- fold $ getEntities database <$> entityTypes
                        let
                            send "types" = Just $ Result $ HasEntityTypes entityTypes
                            send "entities" = Just $ Result $ HasEntities allEntities
                            send _ = Nothing
                        pure send
                    _ -> pure $ const Nothing


instance nodeAtom :: R.Atom Node where
    labelOf = show
    uniqueIdOf = show
    debugInfoOf = show
