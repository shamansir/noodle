module Rpd.Def
    -- TODO: remove `-Def` from inner names
    ( noDefs
    , PatchDef(..), NodeDef(..), InletDef(..), OutletDef(..), ChannelDef(..)
    --, comparePDefs, compareNDefs, compareIDefs, compareODefs
    )
    where

import Prelude

import Effect

import Data.List (List(..))
import Data.Maybe (Maybe)
import Data.Foldable (and)
import Data.Tuple.Nested (type (/\))

import Rpd.Util (type (/->), Flow, PushF)
import Rpd.Path (InletPath, OutletPath, Alias)
import Rpd.Process (ProcessF(..))


noDefs :: forall d. List d
noDefs = Nil


type PatchDef d =
    { alias :: Alias
    , nodeDefs :: List (NodeDef d)
    }
type NodeDef d =
    { alias :: Alias
    , inletDefs :: List (InletDef d)
    , outletDefs :: List (OutletDef d)
    , process :: ProcessF d
    }
type InletDef d =
    { alias :: Alias
    , default :: Maybe d
    --, readonly :: Bool
    --, hidden :: Bool
    --, cold :: Bool
    --, allow :: List String -- channel types. `allows :: InletDef d -> OutletDef d -> Bool`
    , accept :: Maybe (d -> Boolean) -- FIXME: `const true` by default?
    --, adapt :: (d -> d)
    --, tune :: (Event d -> Event d)
    --, show :: (d -> String)
    }
type OutletDef d =
    { alias :: Alias
    , accept :: Maybe (d -> Boolean)
        -- FIXME: `accept : Nothing` reads a bit weird, so I'd try to use `MonadZero.empty`
    }
-- TODO: ChannelDef may be used both to describe inlets and outlets, defaults,
--       accept/decline methods, etc.
type ChannelDef d = InletDef d

-- FIXME: there should always be a string ID, which can be different from name/label:
--        for inlet or outlets it should be unique inside one node that has them
--        for nodes — unique in one toolkit (patch?) etc.


-- comparePDefs :: forall d. Eq d => PatchDef d -> PatchDef d -> Boolean
-- comparePDefs lPDef rPDef =
--     (lPDef.name == rPDef.name)
--       && (and $ compareNDefs <$> lPDef.nodeDefs <*> rPDef.nodeDefs)


-- compareNDefs :: forall d. Eq d => NodeDef d -> NodeDef d -> Boolean
-- compareNDefs lNdef rNdef =
--     (lNdef.name == rNdef.name)
--       && (and $ compareIDefs <$> lNdef.inletDefs <*> rNdef.inletDefs)
--       && (and $ compareODefs <$> lNdef.outletDefs <*> rNdef.outletDefs)


-- compareIDefs :: forall d. Eq d => InletDef d -> InletDef d -> Boolean
-- compareIDefs lIdef rIdef =
--     (lIdef.label == rIdef.label)
--       && (lIdef.default == rIdef.default)


-- compareODefs :: forall d. OutletDef d -> OutletDef d -> Boolean
-- compareODefs lOdef rOdef = lOdef.label == rOdef.label
