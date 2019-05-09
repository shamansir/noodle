module Rpd.Path
    ( mkAlias, getAlias, Alias
    , PatchId(..), NodePath(..), InletPath(..), OutletPath(..), LinkId(..)
    , patchId, nodePath, inletPath, outletPath, linkId
    , getPatchOfNode, getPatchOfInlet, getPatchOfOutlet, getNodeOfInlet, getNodeOfOutlet
    , getPatchId, getNodeId, getInletId, getOutletId -- TODO: rename to ...Idx
    , nodeInPatch, inletInNode, outletInNode
    , Path(..)
    )
    where


import Prelude

import Data.String as String

-- TODO: either Path typeclass or... Comonad? (paths look like breadcrumbs)

-- FIXME: consider moving to random hashes, since adding/removing the things
--        based on IDs breaks, for example, processing (new IDs are assigned
--        following the current number of nodes/inlets/outlets, which is wrong)

-- import Effect (Effect, forE)
-- import Effect.Random (randomInt)


-- uniqueId :: Int -> Effect String
-- uniqueId len =
--     forE 0 len (\_ -> do
--         nextChar <- randomInt 0 96
--         pure unit)



-- class IsAlias


-- Alias is the default or custom name for the entity (patch, node, inlet, outlet, etc.)
-- which is recommended to be unique in this context, but has no guarantees to be so —
-- uniqueness, unlike with UUID, is provided by user.
-- This way the nice-looking paths may be created, like `my-patch/my-node/my-inlet`
-- and so the particular inlet can be referenced with this path
-- after the creation.

-- If there was no Alias specified on entity creation, the alias from the correspoding
-- `Def` is taken and the index in current context is added to it.

-- Still, in the internal


newtype Alias = Alias String


data PatchId = PatchId Alias
data NodePath = NodePath PatchId Alias
data InletPath = InletPath NodePath Alias
data OutletPath = OutletPath NodePath Alias
data LinkId = LinkId Alias


data Path
    = ToNetwork
    | ToPatch PatchId
    | ToNode NodePath
    | ToInlet InletPath
    | ToOutlet OutletPath
    | ToLink LinkId
    | Unknown


mkAlias :: String -> Alias
mkAlias = String.toLower >>> Alias -- TODO: trim spaces, symbols etc.


getAlias :: Alias -> String
getAlias (Alias v) = v


patchId :: Alias -> PatchId
patchId = PatchId


nodePath :: Alias -> Alias -> NodePath
nodePath pId nId = NodePath (PatchId pId) nId


inletPath :: Alias -> Alias -> Alias -> InletPath
inletPath pId nId iId = InletPath (NodePath (PatchId pId) nId) iId


outletPath :: Alias -> Alias -> Alias -> OutletPath
outletPath pId nId iId = OutletPath (NodePath (PatchId pId) nId) iId


linkId :: Alias -> LinkId
linkId = LinkId


getPatchId :: PatchId -> Alias
getPatchId (PatchId id) = id


getNodeId :: NodePath -> Alias
getNodeId (NodePath _ id) = id


getInletId :: InletPath -> Alias
getInletId (InletPath _ id) = id


getOutletId :: OutletPath -> Alias
getOutletId (OutletPath _ id) = id


nodeInPatch :: PatchId -> Alias -> NodePath
nodeInPatch = NodePath


inletInNode :: NodePath -> Alias -> InletPath
inletInNode = InletPath


outletInNode :: NodePath -> Alias -> OutletPath
outletInNode = OutletPath


unpackNodePath :: NodePath -> Array Alias
unpackNodePath (NodePath (PatchId patchId) id) = [ patchId, id ]

unpackInletPath :: InletPath -> Array Alias
unpackInletPath (InletPath nodePath id) = unpackNodePath nodePath <> [ id ]

unpackOutletPath :: OutletPath -> Array Alias
unpackOutletPath (OutletPath nodePath id) = unpackNodePath nodePath <> [ id ]


{-
isNodeInPatch :: NodePath -> PatchId -> Boolean
isNodeInPatch (NodePath patchId' _) patchId =
    patchId == patchId'


isInletInPatch :: InletPath -> PatchId -> Boolean
isInletInPatch (InletPath nodePath _) patchId =
    isNodeInPatch nodePath patchId


isOutletInPatch :: OutletPath -> PatchId -> Boolean
isOutletInPatch (OutletPath nodePath _) patchId =
    isNodeInPatch nodePath patchId


isInletInNode :: InletPath -> NodePath -> Boolean
isInletInNode (InletPath nodePath' _) nodePath =
    nodePath == nodePath'


isOutletInNode :: OutletPath -> NodePath -> Boolean
isOutletInNode (OutletPath nodePath' _) nodePath =
    nodePath == nodePath'


notInTheSameNode :: InletPath -> OutletPath -> Boolean
notInTheSameNode (InletPath iNodePath _) (OutletPath oNodePath _) =
    iNodePath /= oNodePath
-}



-- FIXME: below are Prisms

getPatchOfNode :: NodePath -> PatchId
getPatchOfNode (NodePath pId _) = pId


getPatchOfInlet :: InletPath -> PatchId
getPatchOfInlet inlet = getPatchOfNode $ getNodeOfInlet inlet


getPatchOfOutlet :: OutletPath -> PatchId
getPatchOfOutlet outlet = getPatchOfNode $ getNodeOfOutlet outlet


getNodeOfInlet :: InletPath -> NodePath
getNodeOfInlet  (InletPath nPath _) = nPath


getNodeOfOutlet :: OutletPath -> NodePath
getNodeOfOutlet  (OutletPath nPath _) = nPath


instance showAlias :: Show Alias where
    show (Alias alias) = ":" <> alias <> ":"


instance showPatchId :: Show PatchId where
    show (PatchId id) = "P" <> show id

instance showNodePath :: Show NodePath where
    show (NodePath patchId id) = show patchId <> "/N" <> show id

instance showInletPath :: Show InletPath where
    show (InletPath nodePath id) = show nodePath <> "/I" <> show id

instance showOutletPath :: Show OutletPath where
    show (OutletPath nodePath id) = show nodePath <> "/O" <> show id

instance showLinkId :: Show LinkId where
    show (LinkId id) = "L" <> show id



instance showPath :: Show Path where
    show ToNetwork = "<nw>"
    show (ToPatch patchId) = "<p " <> show patchId <> ">"
    show (ToNode nodePath) = "<n " <> show nodePath <> ">"
    show (ToInlet inletPath) = "<i " <> show inletPath <> ">"
    show (ToOutlet outletPath) = "<o " <> show outletPath <> ">"
    show (ToLink linkId) = "<l " <> show linkId <> ">"
    show Unknown = "<?>"


instance eqAlias :: Eq Alias where
    eq (Alias a) (Alias b) = a == b


instance eqPatchId :: Eq PatchId where
    eq (PatchId a) (PatchId b) = a == b

instance eqNodePath :: Eq NodePath where
    eq (NodePath pa a) (NodePath pb b) = (pa == pb) && (a == b)

instance eqInletPath :: Eq InletPath where
    eq (InletPath na a) (InletPath nb b) = (na == nb) && (a == b)

instance eqOutletPath :: Eq OutletPath where
    eq (OutletPath na a) (OutletPath nb b) = (na == nb) && (a == b)

instance eqLinkId :: Eq LinkId where
    eq (LinkId a) (LinkId b) = a == b


instance ordAlias :: Ord Alias where
    compare (Alias a) (Alias b) = compare a b


instance ordPatchId :: Ord PatchId where
    compare (PatchId a) (PatchId b) = compare a b

instance ordNodePath :: Ord NodePath where
    compare nodePath1 nodePath2 =
        compare (unpackNodePath nodePath1)  (unpackNodePath nodePath2)

instance ordInletPath :: Ord InletPath where
    compare inletPath1 inletPath2 =
        compare (unpackInletPath inletPath1)  (unpackInletPath inletPath2)

instance ordOutletPath :: Ord OutletPath where
    compare outletPath1 outletPath2 =
        compare (unpackOutletPath outletPath1) (unpackOutletPath outletPath2)

instance ordLinkId :: Ord LinkId where
    compare (LinkId a) (LinkId b) =
        compare a b
