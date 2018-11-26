module Rpd.Def
    -- TODO: remove `-Def` from inner names
    ( noDefs
    , PatchDef(..), NodeDef(..), InletDef(..), OutletDef(..), ChannelDef(..)
    , ProcessF(..)
    , comparePDefs, compareNDefs, compareIDefs, compareODefs
    )
    where

import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe)
import Data.Foldable (and)

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
-- TODO: also there can be a `Pipe`d or `Direct` approach, i.e. a function
--       of type (String -> d -> (String /\ d)), where there is no need in other inlet
--       values except one, so it is called for each inlets one by one and so collects
--       values for outputs
data ProcessF d
    = FlowThrough
    | IndexBased (Array d -> Array d)
    | LabelBased ((String /-> d) -> (String /-> d))
    -- TODO: Effectful ProcessF
    -- TODO: Other types


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
    , process :: ProcessF d
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
    }
-- ChannelDef may be used both to describe inlets and outlets
type ChannelDef d = InletDef d



comparePDefs :: forall d. Eq d => PatchDef d -> PatchDef d -> Boolean
comparePDefs lPDef rPDef =
    (lPDef.name == rPDef.name)
      && (and $ compareNDefs <$> lPDef.nodeDefs <*> rPDef.nodeDefs)


compareNDefs :: forall d. Eq d => NodeDef d -> NodeDef d -> Boolean
compareNDefs lNdef rNdef =
    (lNdef.name == rNdef.name)
      && (and $ compareIDefs <$> lNdef.inletDefs <*> rNdef.inletDefs)
      && (and $ compareODefs <$> lNdef.outletDefs <*> rNdef.outletDefs)
      && (lNdef.process == rNdef.process)


compareIDefs :: forall d. Eq d => InletDef d -> InletDef d -> Boolean
compareIDefs lIdef rIdef =
    (lIdef.label == rIdef.label)
      && (lIdef.default == rIdef.default)


compareODefs :: forall d. OutletDef d -> OutletDef d -> Boolean
compareODefs lOdef rOdef = lOdef.label == rOdef.label


instance eqProcessF :: Eq (ProcessF d) where
    eq FlowThrough FlowThrough = true
    eq (IndexBased _) (IndexBased _) = true
    eq (LabelBased _) (LabelBased _) = true
    eq _ _ = false


instance showProcessF :: Show (ProcessF d) where
    -- show (RpdError text) = "(RpdError)" <> text
    show FlowThrough = "FlowThrough"
    show (IndexBased _) = "IndexBased"
    show (LabelBased _) = "LabelBased"
