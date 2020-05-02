module Noodle.Path
    ( Path, Alias
    , toPatch, toNode, toInlet, toOutlet
    , getPatchPath, getNodePath
    , getPatchPath', getNodePath'
    , ToPatch(..), ToNode(..), ToInlet(..), ToOutlet(..)
    , class MarksPath, lift
    , explodePatchPath, explodeNodePath, explodeInletPath, explodeOutletPath
    , nodeInPatch, inletInNode, outletInNode
    )
    where


import Prelude

import Data.List
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))


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


-- TODO: either Path typeclass or... Comonad? (paths look like breadcrumbs)

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


-- infixl 1 joinPaths as @


-- joinPaths :: Path -> Path -> Path
-- joinPaths End End = End
-- joinPaths End (Deeper _ _) = End
-- joinPaths (Deeper alias End) other = Deeper alias other
-- joinPaths (Deeper alias first) second =
    -- where
    --     findLast :: Path -> Path -> Path Path


type Alias = String -- TODO: newtype Alias = Alias String


-- data Path
--     = End
--     | Deeper Alias Path


class MarksPath a where
    lift :: a -> Path


data Path
    = ToPatch' ToPatch
    | ToNode' ToNode
    | ToInlet' ToInlet
    | ToOutlet' ToOutlet


-- class HasPath a where
--     extractPath :: a -> Path


-- TODO: find the general approach as in UUID
newtype ToPatch = ToPatch Alias
newtype ToNode = ToNode { patch :: Alias, node :: Alias }
newtype ToInlet = ToInlet { patch :: Alias, node :: Alias, inlet :: Alias }
newtype ToOutlet = ToOutlet { patch :: Alias, node :: Alias, outlet :: Alias }


derive instance eqToPatch :: Eq ToPatch
derive instance ordToPatch :: Ord ToPatch
derive instance eqToNode :: Eq ToNode
derive instance ordToNode :: Ord ToNode
derive instance eqToInlet :: Eq ToInlet
derive instance ordToInlet :: Ord ToInlet
derive instance eqToOutlet :: Eq ToOutlet
derive instance ordToOutlet :: Ord ToOutlet


instance marksPathToPatch ∷ MarksPath ToPatch where lift = ToPatch'
instance marksPathToNode ∷ MarksPath ToNode where lift = ToNode'
instance marksPathToInlet ∷ MarksPath ToInlet where lift = ToInlet'
instance marksPathToOutlet ∷ MarksPath ToOutlet where lift = ToOutlet'


-- empty :: Path
-- empty = End


toPatch :: Alias -> ToPatch
toPatch = ToPatch


toNode :: Alias -> Alias -> ToNode
toNode patch node = ToNode { patch, node }


toInlet :: Alias -> Alias -> Alias -> ToInlet
toInlet patch node inlet = ToInlet { patch, node, inlet }


toOutlet :: Alias -> Alias -> Alias -> ToOutlet
toOutlet patch node outlet = ToOutlet { patch, node, outlet }


getPatchPath :: Path -> ToPatch
getPatchPath (ToPatch' (ToPatch alias)) = toPatch alias
getPatchPath (ToNode' (ToNode { patch })) = toPatch patch
getPatchPath (ToInlet' (ToInlet { patch })) = toPatch patch
getPatchPath (ToOutlet' (ToOutlet { patch })) = toPatch patch


getPatchPath' :: Path -> Path
getPatchPath' = ToPatch' <<< getPatchPath


getNodePath :: Path -> Maybe ToNode
getNodePath (ToPatch' _) = Nothing
getNodePath (ToNode' (ToNode { patch, node })) = Just $ toNode patch node
getNodePath (ToInlet' (ToInlet { patch, node })) = Just $ toNode patch node
getNodePath (ToOutlet' (ToOutlet { patch, node })) = Just $ toNode patch node


getNodePath' :: Path -> Maybe Path
getNodePath' p = ToNode' <$> getNodePath p


explodePatchPath :: ToPatch -> Alias
explodePatchPath (ToPatch alias) = alias


explodeNodePath :: ToNode -> Alias /\ Alias
explodeNodePath (ToNode { patch, node }) = patch /\ node


explodeInletPath :: ToInlet -> Alias /\ Alias /\ Alias
explodeInletPath (ToInlet { patch, node, inlet }) = patch /\ node /\ inlet


explodeOutletPath :: ToOutlet -> Alias /\ Alias /\ Alias
explodeOutletPath (ToOutlet { patch, node, outlet }) = patch /\ node /\ outlet


nodeInPatch :: ToPatch -> Alias -> ToNode
nodeInPatch (ToPatch patch) node =
    ToNode
        { patch, node }


inletInNode :: ToNode -> Alias -> ToInlet
inletInNode (ToNode { patch, node }) inlet =
    ToInlet
        { patch, node, inlet }


outletInNode :: ToNode -> Alias -> ToOutlet
outletInNode (ToNode { patch, node }) outlet =
    ToOutlet
        { patch, node, outlet }


instance showToPatch :: Show ToPatch where
    show (ToPatch alias) =
        "<P@" <> alias <> ">"

instance showToNode :: Show ToNode where
    show (ToNode { patch, node }) =
        "<P@" <> patch <> "/N@" <> node <> ">"

instance showToInlet :: Show ToInlet where
    show (ToInlet { patch, node, inlet }) =
        "<P@" <> patch <> "/N@" <> node <> "/@I" <> inlet <> ">"

instance showToOutlet :: Show ToOutlet where
    show (ToOutlet { patch, node, outlet }) =
        "<P@" <> patch <> "/N@" <> node <> "/@O" <> outlet <> ">"


instance showPath :: Show Path where
    show (ToPatch' p)  = "<" <> show p <> ">"
    show (ToNode' n)   = "<" <> show n <> ">"
    show (ToInlet' i)  = "<" <> show i <> ">"
    show (ToOutlet' o) = "<" <> show o <> ">"

derive instance eqPath :: Eq Path
derive instance ordPath :: Ord Path

