module Noodle.Channel
  ( Def, Id, make
  , default, isHidden, id
  , hot, cold, hiddenHot, hiddenCold
  )
  where


import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)


type Id = String


data Temperature
    = Hot
    | Cold


data Visibility
    = Hidden
    | Visible


newtype Def d =
    Def
    { id :: Id
    , default :: d
    , temperature :: Temperature
    , visibility :: Visibility
    }


instance functorDef :: Functor Def where
    map f (Def def) =
        { id : def.id
        , default : f def.default
        -- , adapt : ?wh def.adapt
        , temperature : def.temperature
        , visibility : def.visibility
        } # Def


make :: forall d. Id -> d -> Def d
make = hot


default :: forall d. Def d -> d
default (Def def) = def.default -- unwrap >>> _.default


isHidden :: forall d. Def d -> Boolean
isHidden (Def def) =
    case def.visibility of
        Hidden -> true
        Visible -> false


id :: forall d. Def d -> Id
id (Def { id }) = id


hot :: forall d. Id -> d -> Def d
hot id def =
    Def
        { id
        , default : def
        , temperature : Hot
        , visibility : Visible
        }


cold :: forall d. Id -> d -> Def d
cold id def =
    Def
        { id
        , default : def
        , temperature : Cold
        , visibility : Visible
        }


hiddenHot :: forall d. Id -> d -> Def d
hiddenHot id def =
    Def
        { id
        , default : def
        , temperature : Hot
        , visibility : Hidden
        }


hiddenCold :: forall d. Id -> d -> Def d
hiddenCold id def =
    Def
        { id
        , default : def
        , temperature : Cold
        , visibility : Hidden
        }


{-
class Channel d where
    --adapt :: d -> d
    accept :: d -> Maybe d
    id :: forall d. String


type FullDef d =
    { id :: String
    , default :: d -- d?
    --, accept :: d -> Maybe a -- TODO: remove, move to typeclass, can be used on `Node.connect` / `Node.send`
    --, adapt :: d -> d
    , temperature :: Temperature
    , visibility :: Visibility
    }
-}