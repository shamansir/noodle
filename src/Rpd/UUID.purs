module Rpd.UUID
    ( UUID
    , new
    , toString
    , Tagged
    , ToPatch(..), ToNode(..), ToInlet(..), ToOutlet(..), ToLink(..)
    , Tag, liftTagged, class IsTagged, tag, uuid
    ) where


import Prelude
import Effect (Effect)


import Data.Foldable (fold)
import Data.Map


foreign import newAsString :: Effect String


newtype UUID = UUID String

derive instance eqUuid :: Eq UUID
derive instance ordUuid :: Ord UUID


new :: Effect UUID
new = newAsString <#> UUID


toString :: UUID -> String
toString (UUID uuid) = uuid


instance showUUID :: Show UUID where
  show (UUID uuid) = "{" <> uuid <> "}"



newtype ToPatch  = ToPatch UUID
newtype ToNode = ToNode UUID
newtype ToInlet  = ToInlet UUID
newtype ToOutlet = ToOutlet UUID
newtype ToLink = ToLink UUID


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

liftTagged :: forall a . IsTagged a => a -> Tagged
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


-- mymap =
--   insert    ( liftTagged $ ToNode "node1" ) "node1Val" $
--   insert    ( liftTagged $ ToNode "node2" ) "node2Val" $
--   singleton ( liftTagged $ ToPatch "patch1" ) "patch1Val"


-- main :: Eff (dom :: DOM) Unit
-- main =
--     render $ fold
--       [ h1 ( text $ show $ lookup ( liftTagged $ ToNode "node1" ) mymap )
--       , h1 ( text $ show $ lookup ( liftTagged $ ToNode "node2" ) mymap )
--       , h1 ( text $ show $ lookup ( liftTagged $ ToPatch "node2" ) mymap )
--       , h1 ( text $ show $ lookup ( liftTagged $ ToPatch "patch1" ) mymap )
--       ]
