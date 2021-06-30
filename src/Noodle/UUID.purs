module Noodle.UUID
    ( UUID
    , new
    , toRawString, taggedToRawString
    , Tagged
    , ToPatch(..), ToNode(..), ToInlet(..), ToOutlet(..), ToLink(..)
    , Tag, liftTagged, class IsTagged, tag, uuid
    , toPatch, toNode, toInlet, toOutlet, toLink
    , isToPatch, isToNode, isToInlet, isToOutlet, isToLink
    , encode, decode
    ) where


import Prelude
import Data.Maybe
import Effect (Effect)


foreign import newAsString :: Effect String


newtype UUID = UUID String

derive instance eqUuid :: Eq UUID
derive instance ordUuid :: Ord UUID


new :: Effect UUID
new = newAsString <#> UUID


toRawString :: UUID -> String
toRawString (UUID uuid) = uuid


taggedToRawString :: forall a. IsTagged a => a -> String
taggedToRawString tagged = toRawString $ uuid tagged


instance showUUID :: Show UUID where
  show (UUID uuid) = "{" <> uuid <> "}"


newtype ToPatch  = ToPatch UUID
newtype ToNode = ToNode UUID
newtype ToInlet  = ToInlet UUID
newtype ToOutlet = ToOutlet UUID
newtype ToLink = ToLink UUID


instance showToPatch :: Show ToPatch where
  show (ToPatch (UUID uuid))  = "{P@" <> uuid <> "}"

instance showToNode :: Show ToNode where
  show (ToNode (UUID uuid))   = "{N@" <> uuid <> "}"

instance showToInlet :: Show ToInlet where
  show (ToInlet (UUID uuid))  = "{I@" <> uuid <> "}"

instance showToOutlet :: Show ToOutlet where
  show (ToOutlet (UUID uuid)) = "{O@" <> uuid <> "}"

instance showToLink :: Show ToLink where
  show (ToLink (UUID uuid))   = "{L@" <> uuid <> "}"


derive instance eqToPatch :: Eq ToPatch
derive instance ordToPatch :: Ord ToPatch
derive instance eqToNode :: Eq ToNode
derive instance ordToNode :: Ord ToNode
derive instance eqToInlet :: Eq ToInlet
derive instance ordToInlet :: Ord ToInlet
derive instance eqToOutlet :: Eq ToOutlet
derive instance ordToOutlet :: Ord ToOutlet
derive instance eqToLink :: Eq ToLink
derive instance ordToLink :: Ord ToLink


data Tag
  = ToPatchT
  | ToNodeT
  | ToInletT
  | ToOutletT
  | ToLinkT


derive instance eqTag :: Eq Tag
derive instance ordTag :: Ord Tag


class IsTagged a where
  tag  :: a -> Tag
  uuid :: a -> UUID


instance isTaggedToPatch :: IsTagged ToPatch where
  tag = const ToPatchT
  uuid ( ToPatch u ) = u

instance isTaggedToNode :: IsTagged ToNode where
  tag = const ToNodeT
  uuid ( ToNode u ) = u

instance isTaggedToInlet :: IsTagged ToInlet where
  tag = const ToInletT
  uuid ( ToInlet u ) = u

instance isTaggedToOutlet :: IsTagged ToOutlet where
  tag = const ToOutletT
  uuid ( ToOutlet u ) = u

instance isTaggedToLink :: IsTagged ToLink where
  tag = const ToLinkT
  uuid ( ToLink u ) = u


newtype Tagged = Tagged ( forall r . ( forall a . IsTagged a => a -> r ) -> r )

runTagged ( Tagged f ) = f

liftTagged :: forall a. IsTagged a => a -> Tagged
liftTagged v = Tagged \ f -> f v


instance isTaggedTagged :: IsTagged Tagged where
  tag c  = runTagged c tag
  uuid c = runTagged c uuid

instance eqEntity :: Eq Tagged where
  eq a b = eq ( tag a ) ( tag b ) && eq ( uuid a ) ( uuid b )

instance ordEntity :: Ord Tagged where
  compare a b = case compare ( tag a ) ( tag b ) of
    GT -> GT
    LT -> LT
    EQ -> compare ( uuid a ) ( uuid b )


toPatch :: Tagged -> Maybe ToPatch
toPatch x | tag x == ToPatchT = Just $ ToPatch $ uuid x
toPatch x | otherwise = Nothing


toNode :: Tagged -> Maybe ToNode
toNode x | tag x == ToNodeT = Just $ ToNode $ uuid x
toNode x | otherwise = Nothing


toInlet :: Tagged -> Maybe ToInlet
toInlet x | tag x == ToInletT = Just $ ToInlet $ uuid x
toInlet x | otherwise = Nothing


toOutlet :: Tagged -> Maybe ToOutlet
toOutlet x | tag x == ToOutletT = Just $ ToOutlet $ uuid x
toOutlet x | otherwise = Nothing


toLink :: Tagged -> Maybe ToLink
toLink x | tag x == ToLinkT = Just $ ToLink $ uuid x
toLink x | otherwise = Nothing


isToPatch :: Tagged -> Boolean
isToPatch = toPatch >>> isJust


isToNode :: Tagged -> Boolean
isToNode = toNode >>> isJust


isToInlet :: Tagged -> Boolean
isToInlet = toNode >>> isJust


isToOutlet :: Tagged -> Boolean
isToOutlet = toOutlet >>> isJust


isToLink :: Tagged -> Boolean
isToLink = toLink >>> isJust


encode :: Tagged -> { uuid :: String, kind :: String }
encode tagged
  = { uuid : toRawString $ uuid tagged
    , kind :
        case tag tagged of
          ToPatchT -> "patch"
          ToNodeT -> "node"
          ToInletT -> "inlet"
          ToOutletT -> "outlet"
          ToLinkT -> "link"
    }


decode :: { uuid :: String, kind :: String } -> Maybe Tagged
decode v =
  case v.kind of
    "patch"  -> Just $ liftTagged $ ToPatch  $ UUID v.uuid
    "node"   -> Just $ liftTagged $ ToNode   $ UUID v.uuid
    "inlet"  -> Just $ liftTagged $ ToInlet  $ UUID v.uuid
    "outlet" -> Just $ liftTagged $ ToOutlet $ UUID v.uuid
    "link"   -> Just $ liftTagged $ ToLink   $ UUID v.uuid
    _ -> Nothing
