module Rpd.Path
    ( Path, Alias
    , toPatch, toNode, toInlet, toOutlet
    , getPatchPath, getNodePath
    , getPatchPath', getNodePath'
    , ToPatch, ToNode, ToInlet, ToOutlet
    , class MarksPath, test
    )
    where


import Prelude

import Data.List
import Data.Maybe


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
    test :: a -> Maybe Path


data Path
    = ToPatch' Alias
    | ToNode' { patch :: Alias, node :: Alias }
    | ToInlet' { patch :: Alias, node :: Alias, inlet :: Alias }
    | ToOutlet' { patch :: Alias, node :: Alias, outlet :: Alias }


-- class HasPath a where
--     extractPath :: a -> Path


-- TODO: find the general approach as in UUID
newtype ToPatch = ToPatch Path
newtype ToNode = ToNode Path
newtype ToInlet = ToInlet Path
newtype ToOutlet = ToOutlet Path


instance marksPathToPatch ∷ MarksPath ToPatch where
    test (ToPatch p@(ToPatch' _)) = Just p
    test _ = Nothing

instance marksPathToNode ∷ MarksPath ToNode where
    test (ToNode p@(ToNode' _)) = Just p
    test _ = Nothing

instance marksPathToInlet ∷ MarksPath ToInlet where
    test (ToInlet p@(ToInlet' _)) = Just p
    test _ = Nothing

instance marksPathToOutlet ∷ MarksPath ToOutlet where
    test (ToOutlet p@(ToOutlet' _)) = Just p
    test _ = Nothing


-- empty :: Path
-- empty = End


toPatch :: Alias -> ToPatch
toPatch palias =
    ToPatch $ ToPatch' palias


toNode :: Alias -> Alias -> ToNode
toNode palias nalias =
    ToNode $ ToNode' { patch : palias, node : nalias }


toInlet :: Alias -> Alias -> Alias -> ToInlet
toInlet palias nalias ialias =
    ToInlet $ ToInlet' { patch : palias, node : nalias, inlet : ialias }


toOutlet :: Alias -> Alias -> Alias -> ToOutlet
toOutlet palias nalias oalias =
    ToOutlet $ ToOutlet' { patch : palias, node : nalias, outlet : oalias }


getPatchPath :: Path -> ToPatch
getPatchPath (ToPatch' alias) = toPatch alias
getPatchPath (ToNode' { patch }) = toPatch patch
getPatchPath (ToInlet' { patch }) = toPatch patch
getPatchPath (ToOutlet' { patch }) = toPatch patch


getPatchPath' :: Path -> Path
getPatchPath' (ToPatch' alias) = ToPatch' alias
getPatchPath' (ToNode' { patch }) = ToPatch' patch
getPatchPath' (ToInlet' { patch }) = ToPatch' patch
getPatchPath' (ToOutlet' { patch }) = ToPatch' patch


getNodePath :: Path -> Maybe ToNode
getNodePath (ToPatch' _) = Nothing
getNodePath (ToNode' { patch, node }) = Just $ toNode patch node
getNodePath (ToInlet' { patch, node }) = Just $ toNode patch node
getNodePath (ToOutlet' { patch, node }) = Just $ toNode patch node


getNodePath' :: Path -> Maybe Path
getNodePath' (ToPatch' _) = Nothing
getNodePath' (ToNode' { patch, node }) = Just $ ToNode' { patch, node }
getNodePath' (ToInlet' { patch, node }) = Just $ ToNode' { patch, node }
getNodePath' (ToOutlet' { patch, node }) = Just $ ToNode' { patch, node }


instance showPath :: Show Path where
    show (ToPatch' alias) =
        "<P@" <> alias <> ">"
    show (ToNode' { patch, node }) =
        "<P@" <> patch <> "/N@" <> node <> ">"
    show (ToInlet' { patch, node, inlet }) =
        "<P@" <> patch <> "/N@" <> node <> "/@I" <> inlet <> ">"
    show (ToOutlet' { patch, node, outlet }) =
        "<P@" <> patch <> "/N@" <> node <> "/@O" <> outlet <> ">"


derive instance eqPath :: Eq Path
derive instance ordPath :: Ord Path

