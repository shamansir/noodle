module Front.Shared.Bounds where

import Prelude


type Bounds_ n =
    { left   :: n
    , top    :: n
    , width  :: n
    , height :: n
    }


type Size_ n =
    { width  :: n
    , height :: n
    }


type Position_ n =
    { left :: n
    , top  :: n
    }


type PositionXY_ n =
    { x :: n
    , y :: n
    }


type Delta_ n =
    { dx :: n
    , dy :: n
    }


type Position   = Position_   Number
type PositionXY = PositionXY_ Number
type Bounds     = Bounds_     Number
type Size       = Size_       Number
type Delta      = Delta_      Number


type IntPosition   = Position_   Int
type IntPositionXY = PositionXY_ Int
type IntBounds     = Bounds_     Int
type IntSize       = Size_       Int
type IntDelta      = Delta_      Int


getPosition :: Bounds -> Position
getPosition { left, top } = { left, top }


getSize :: Bounds -> Size
getSize { width, height } = { width, height }


zeroBounds :: Bounds
zeroBounds = { top : 0.0, left : 0.0, width : 0.0, height : 0.0 }