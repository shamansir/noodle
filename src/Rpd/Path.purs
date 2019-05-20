module Rpd.Path
    ( Path(..), Alias
    , empty, toPatch, toNode, toInlet, toOutlet
    , length
    , mayLeadToPatch, mayLeadToNode, mayLeadToInlet, mayLeadToOutlet
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


data Path
    = End
    | Deeper Alias Path


empty :: Path
empty = End


toPatch :: Alias -> Path
toPatch palias =
    Deeper palias End


toNode :: Alias -> Alias -> Path
toNode palias nalias =
    Deeper palias $ Deeper nalias End


toInlet :: Alias -> Alias -> Alias -> Path
toInlet palias nalias ialias =
    Deeper palias $ Deeper nalias $ Deeper ialias End


toOutlet :: Alias -> Alias -> Alias -> Path
toOutlet = toInlet


-- flatten :: Path -> List Alias
-- flatten End = []
-- flatten (Deeper alias p) = alias :: flatten p


length :: Path -> Int
length End = 0
length (Deeper alias p) = 1 + length p


mayLeadToPatch :: Path -> Boolean
mayLeadToPatch p = length p == 1


mayLeadToNode :: Path -> Boolean
mayLeadToNode p = length p == 2


mayLeadToInlet :: Path -> Boolean
mayLeadToInlet p = length p == 3


mayLeadToOutlet :: Path -> Boolean
mayLeadToOutlet p = length p == 3


getPatchPath :: Path -> Maybe Path
getPatchPath End = Nothing
getPatchPath (Deeper alias _) = Just $ Deeper alias End


getNodePath :: Path -> Maybe Path
getNodePath (Deeper pAlias (Deeper nAlias _)) =
    Just $ Deeper pAlias (Deeper nAlias End)
getNodePath _ = Nothing


instance showPath :: Show Path where
    show End = ""
    show (Deeper alias End) = alias
    show (Deeper alias p) = alias <> "/" <> show p


instance eqPath :: Eq Path where
    eq End End = true
    eq (Deeper aliasL pL) (Deeper aliasR pR) = aliasL == aliasR && pL == pR
    eq _ _ = false


instance ordPath :: Ord Path where
    compare End End = EQ
    compare End (Deeper _ _) = LT
    compare (Deeper _ _) End = GT
    compare (Deeper aliasL pL) (Deeper aliasR pR) | compare pL pR == EQ = compare aliasL aliasR
    compare (Deeper aliasL pL) (Deeper aliasR pR) | otherwise = compare pL pR
        -- case compare pL pR of
        --     EQ -> compare aliasL aliasR
        --     _ -> compare pL pR


-- -- derive instance genericAlias :: Generic Alias _
-- -- instance eqAlias :: Eq Alias where
-- --   eq = GEq.genericEq
-- -- instance ordAlias :: Ord Alias where
-- --   compare = GOrd.genericCompare


-- -- derive instance genericPath :: Generic Path _
-- -- instance eqPath :: Eq Path where
-- --   eq = GEq.genericEq

