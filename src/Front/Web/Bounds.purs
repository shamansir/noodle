module Web.Bounds where

import Prelude


type Bounds =
    { left :: Number
    , top :: Number
    , width :: Number
    , height :: Number
    }


getPosition :: Bounds -> { left :: Number, top :: Number }
getPosition { left, top } = { left, top }


getSize :: Bounds -> { width :: Number, height :: Number }
getSize { width, height } = { width, height }