module Rpd.Path
    ( Path(..), Alias
    , toPatch, toNode, toInlet, toOutlet
    , getPatchPath, getNodePath
    )
    where


import Prelude

import Data.List
import Data.Maybe

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd


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


data Path
    = ToPatch Alias
    | ToNode { patch :: Alias, node :: Alias }
    | ToInlet { patch :: Alias, node :: Alias, inlet :: Alias }
    | ToOutlet { patch :: Alias, node :: Alias, outlet :: Alias }


-- empty :: Path
-- empty = End


toPatch :: Alias -> Path
toPatch palias =
    ToPatch palias


toNode :: Alias -> Alias -> Path
toNode palias nalias =
    ToNode { patch : palias, node : nalias }


toInlet :: Alias -> Alias -> Alias -> Path
toInlet palias nalias ialias =
    ToInlet { patch : palias, node : nalias, inlet : ialias }


toOutlet :: Alias -> Alias -> Alias -> Path
toOutlet palias nalias oalias =
    ToOutlet { patch : palias, node : nalias, outlet : oalias }


-- flatten :: Path -> List Alias
-- flatten End = []
-- flatten (Deeper alias p) = alias :: flatten p


-- length :: Path -> Int
-- length = accLength 0
--     where
--         accLength acc End = acc
--         accLength acc (Deeper _ p) = accLength (acc+1) p


-- mayLeadToPatch :: Path -> Boolean
-- mayLeadToPatch p = length p == 1


-- mayLeadToNode :: Path -> Boolean
-- mayLeadToNode p = length p == 2


-- mayLeadToInlet :: Path -> Boolean
-- mayLeadToInlet p = length p == 3


-- mayLeadToOutlet :: Path -> Boolean
-- mayLeadToOutlet p = length p == 3


getPatchPath :: Path -> Path
getPatchPath (ToPatch alias) = ToPatch alias
getPatchPath (ToNode { patch }) = ToPatch patch
getPatchPath (ToInlet { patch }) = ToPatch patch
getPatchPath (ToOutlet { patch }) = ToPatch patch


getNodePath :: Path -> Maybe Path
getNodePath (ToPatch _) = Nothing
getNodePath (ToNode { patch, node }) = Just $ ToNode { patch, node }
getNodePath (ToInlet { patch, node }) = Just $ ToNode { patch, node }
getNodePath (ToOutlet { patch, node }) = Just $ ToNode { patch, node }


instance showPath :: Show Path where
    show (ToPatch alias) =
        "<P@" <> alias <> ">"
    show (ToNode { patch, node }) =
        "<P@" <> patch <> "/N@" <> node <> ">"
    show (ToInlet { patch, node, inlet }) =
        "<P@" <> patch <> "/N@" <> node <> "/@I" <> inlet <> ">"
    show (ToOutlet { patch, node, outlet }) =
        "<P@" <> patch <> "/N@" <> node <> "/@O" <> outlet <> ">"


-- instance eqPath :: Eq Path where
--     eq End End = true
--     eq (Deeper aliasL pL) (Deeper aliasR pR) = aliasL == aliasR && pL == pR
--     eq _ _ = false


-- instance ordPath :: Ord Path where
--     compare (ToPatch aliasL) (ToPatch aliasR) = compare aliasL aliasR
--     compare (ToPatch aliasL) _ = LT
--     compare End (Deeper _ _) = LT
--     compare (Deeper _ _) End = GT
--     compare (Deeper aliasL pL) (Deeper aliasR pR) | compare pL pR == EQ = compare aliasL aliasR
--     compare (Deeper aliasL pL) (Deeper aliasR pR) | otherwise = compare pL pR
        -- case compare pL pR of
        --     EQ -> compare aliasL aliasR
        --     _ -> compare pL pR


derive instance eqPath :: Eq Path
derive instance ordPath :: Ord Path

-- -- derive instance genericPath :: Generic Path _
-- -- instance eqPath :: Eq Path where
-- --   eq = GEq.genericEq

