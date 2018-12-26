module Rpd.Def
    -- TODO: remove `-Def` from inner names
    ( noDefs
    , PatchDef(..), NodeDef(..), InletDef(..), OutletDef(..), ChannelDef(..)
    , ProcessF, InletsFlow(..), OutletsFlow(..)
    , comparePDefs, compareNDefs, compareIDefs, compareODefs
    )
    where

import Prelude

import Effect

import Data.List (List(..))
import Data.Maybe (Maybe)
import Data.Foldable (and)
import Data.Tuple.Nested (type (/\))

import Rpd.Util (type (/->), Flow, PushF)
import Rpd.Path (InletPath, OutletPath)

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
-- TODO: generalize Process function to receiving something from incoming data stream and
--       sending something to outgoing data stream, so all the types of `ProcessF`` could
--       implement the same type. The question is — we need folds to store previous values,
--       will we be able to apply them with this implementation?
-- TODO: also there can be a `Pipe`d or `Direct` approach, i.e. a function
--       of type (String -> d -> (String /\ d)), where there is no need in other inlet
--       values except one, so it is called for each inlets one by one and so collects
--       values for outputs

-- data ProcessF d
--     = ByLabel (Flow (String /\ d) -> PushF (String /\ d) -> Effect Unit)
--     | ByPath (Flow (InletPath /\ d) -> PushF (OutletPath /\ d) -> Effect Unit)
    -- | Full (Flow (InletPath /\ InletDef d /\ d) -> PushF (OutletPath /\ d) -> Effect Unit)


data InletsFlow d = InletsFlow (Flow (Int /\ d))
data OutletsFlow d = OutletsFlow (Flow (Int /\ d))
type ProcessF d = InletsFlow d -> OutletsFlow d

-- TODO: some "data flow" typeclass which provides functions like:
-- `receive inletIndex -> Rpd/Effect d`,
-- `send outletIndex data -> Rpd/Effect Unit`,
-- `receive' inletLabel -> Rpd/Effect d`,
-- `send' outletLabel data -> Rpd/Effect Unit`,
-- and maybe... the `Rpd d`, `Network (Node d)` or the `Node d` should implement it,
-- for the `Node` case — it can use `_nodeInlet'`/`_nodeOutlet'` lensed and so
-- search only for the inlets inside, by label

-- data DataSource d
--     = UserSource (Flow d)
--     | OutletSource OutletPath (Flow d)


noDefs :: forall d. List d
noDefs = Nil


type PatchDef d =
    { name :: String
    , nodeDefs :: List (NodeDef d)
    }
type NodeDef d =
    { name :: String
    , inletDefs :: List (InletDef d)
    , outletDefs :: List (OutletDef d)
    , process :: Maybe (ProcessF d)
    }
type InletDef d =
    { label :: String
    , default :: Maybe d
    --, readonly :: Bool
    --, hidden :: Bool
    --, cold :: Bool
    --, allow :: List String -- channel types. `allows :: InletDef d -> OutletDef d -> Bool`
    , accept :: Maybe (d -> Boolean)
    --, adapt :: (d -> d)
    --, tune :: (Event d -> Event d)
    --, show :: (d -> String)
    }
type OutletDef d =
    { label :: String
    , accept :: Maybe (d -> Boolean)
        -- FIXME: `accept : Nothing` reads a bit weird, so I use `MonadZero.empty`
    }
-- ChannelDef may be used both to describe inlets and outlets
type ChannelDef d = InletDef d

-- FIXME: there should always be a string ID, which can be different from name/label:
--        for inlet or outlets it should be unique inside one node that has them
--        for nodes — unique in one toolkit (patch?) etc.


comparePDefs :: forall d. Eq d => PatchDef d -> PatchDef d -> Boolean
comparePDefs lPDef rPDef =
    (lPDef.name == rPDef.name)
      && (and $ compareNDefs <$> lPDef.nodeDefs <*> rPDef.nodeDefs)


compareNDefs :: forall d. Eq d => NodeDef d -> NodeDef d -> Boolean
compareNDefs lNdef rNdef =
    (lNdef.name == rNdef.name)
      && (and $ compareIDefs <$> lNdef.inletDefs <*> rNdef.inletDefs)
      && (and $ compareODefs <$> lNdef.outletDefs <*> rNdef.outletDefs)


compareIDefs :: forall d. Eq d => InletDef d -> InletDef d -> Boolean
compareIDefs lIdef rIdef =
    (lIdef.label == rIdef.label)
      && (lIdef.default == rIdef.default)


compareODefs :: forall d. OutletDef d -> OutletDef d -> Boolean
compareODefs lOdef rOdef = lOdef.label == rOdef.label
