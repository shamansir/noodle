module Data.Vec2 where


import Prelude (flip, class Semiring, class Ord)
import Prelude ((+), (*), (<=), (>=), (&&))
import Prelude (zero) as P

import Data.Vec (Vec, (+>))
import Data.Vec as Vec

import Data.Typelevel.Num.Reps (D2, d0, d1)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Data.EuclideanRing (class EuclideanRing)
import Data.EuclideanRing (div) as ER


type Vec2 = Vec2_ Number
type Vec2_ n = Vec D2 n


type Pos = Vec2
type Pos_ n = Vec2_ n
type Size = Vec2
type Size_ n = Vec2_ n


--derive instance Newtype Position _

zero :: Vec2
zero = 0.0 <+> 0.0


make :: forall n. n -> n -> Vec2_ n
make x y = x +> y +> Vec.empty


vv :: forall n. n -> Vec2_ n
vv n = make n n


xy :: forall n. n -> Pos_ n
xy = vv


square :: forall n. n -> Size_ n
square = vv


toTuple :: forall n. Vec2_ n -> n /\ n
toTuple p = x p /\ y p


fromTuple :: forall n. n /\ n -> Vec2_ n
fromTuple = uncurry make


{-| get X component of a vector. -}
x :: forall n. Pos_ n -> n
x = flip Vec.index d0


{-| create a vector `(x, 0)`. -}
x' :: forall n. Semiring n => n -> Pos_ n
x' x = x <+> P.zero


{-| get Y component of a vector. -}
y :: forall n. Pos_ n -> n
y = flip Vec.index d1


{-| create a vector `(0, y)`. -}
y' :: forall n. Semiring n => n -> Pos_ n
y' y = P.zero <+> y


{-| get left component of a vector. -}
l :: forall n. Vec2_ n -> n
l = x


{-| create a vector `(l, 0)`. -}
l' :: forall n. Semiring n => n -> Vec2_ n
l' = x'


{-| get right component of a vector. -}
r :: forall n. Vec2_ n -> n
r = y


{-| create a vector `(0, r)`. -}
r' :: forall n. Semiring n => n -> Vec2_ n
r' = y'


{-| get width from a vector. -}
w :: forall n. Size_ n -> n
w = x


{-| create a vector `(w, 0)`. -}
w' :: forall n. Semiring n => n -> Size_ n
w' = x'


{-| get height from a vector. -}
h :: forall n. Size_ n -> n
h = y


{-| create a vector `(0, h)`. -}
h' :: forall n. Semiring n => n -> Size_ n
h' = y'


{-| set Y component of a vector to zero: (x, y) -> (x, 0) -}
xz :: forall n. Semiring n => Vec2_ n -> Pos_ n
xz v = x v <+> P.zero


{-| set X component of a vector to zero: (x, y) -> (0, y) -}
zy :: forall n. Semiring n => Vec2_ n -> Pos_ n
zy v = P.zero <+> y v


{-| set height component of a vector to zero: (w, h) -> (w, 0) -}
wz :: forall n. Semiring n => Vec2_ n -> Size_ n
wz = xz


{-| set width component of a vector to zero: (w, h) -> (0, h) -}
zh :: forall n. Semiring n => Vec2_ n -> Size_ n
zh = zy


{-| set right component of a vector to zero: (l, r) -> (l, 0) -}
lz :: forall n. Semiring n => Vec2_ n -> Vec2_ n
lz = xz


{-| set left component of a vector to zero: (l, r) -> (0, r) -}
zr :: forall n. Semiring n => Vec2_ n -> Vec2_ n
zr = zy


area :: forall n. Semiring n => Size_ n -> n
area v = w v * h v


inside :: forall n. Semiring n => Ord n => Pos_ n -> (Pos_ n /\ Size_ n) -> Boolean
inside pos (bpos /\ bsize) =
    px >= bx && px <= bx + bw && py >= by && py <= by + bh
    where
        px = x pos
        py = y pos
        bx = x bpos
        by = y bpos
        bw = w bsize
        bh = h bsize


inside' :: forall n. Semiring n => Ord n => Pos_ n -> Size_ n -> Boolean
inside' pos size = inside pos (P.zero /\ size)


infixl 1 make as <+> -- same as `Semiring`: `<>`


infixl 7 div as </>



div :: forall n. EuclideanRing n => Vec2_ n -> Vec2_ n -> Vec2_ n
div v1 v2 = ER.div (x v1) (x v2) <+> ER.div (y v1) (y v2)


{- instance euclVec2 :: EuclideanRing (Vec D2 Number) where
    degree _ = 1
    div v1 v2 =
        div (x v1) (x v2) <+> div (y v1) (y v2)
    mod _ _ = 0.0 -}


{- instance showPosition :: Show Position where
    show pos = (show $ getX pos) <> ":" <> (show $ getY pos) -}