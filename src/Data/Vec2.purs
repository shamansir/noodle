module Data.Vec2 where


import Prelude (($), flip, show, class Show, (<>), (>>>))
import Data.Vec (Vec, (+>), empty)
import Data.Vec as Vec
import Data.Newtype (class Newtype, unwrap)

import Data.Typelevel.Num.Reps (D2, d0, d1)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))


type Vec2 = Vec D2 Number


--derive instance Newtype Position _



make :: Number -> Number -> Vec2
make x y = x +> y +> empty


toTuple :: Vec2 -> Number /\ Number
toTuple p = getX p /\ getY p


fromTuple :: Number /\ Number -> Vec2
fromTuple = uncurry make


getX :: Vec2 -> Number
getX = flip Vec.index d0


getY :: Vec2 -> Number
getY = flip Vec.index d1


{- instance showPosition :: Show Position where
    show pos = (show $ getX pos) <> ":" <> (show $ getY pos) -}