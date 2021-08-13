module App.Style.Hydra.Units
    (units) where


import Data.Vec2 ((<+>))
import Data.Tuple.Nested ((/\))

import Data.Set (Set)

import App.Style (Units, NodeFlow(..))


{- data Size a = Size Int Int a


data Pixels = Pixels


data Cells = Cells -}


units :: NodeFlow -> Units
units _ =
    { cell :
        { size : 40.0 <+> 40.0
        }
    , body :
        { size : 100.0
        , strokeWidth : 1.0
        , cornerRadius : 0.0
        , margin : 50.0 <+> 10.0
        }
    , title :
        { size : 20.0
        , padding : 3.0 <+> 10.0
        }
    , slot :
        { radius : 3.5
        , strokeWidth : 1.5
        , area : 50.0 <+> 25.0 -- x, same as body.margin
        , inletsOffset : 0.0 <+> 0.0
        , outletsOffset : 0.0 <+> 0.0
        }
    }
