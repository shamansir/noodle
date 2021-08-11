module App.Style.Hydra.Units
    (units) where


import Data.Vec2 ((<+>))

import App.Style (Units, Side(..), NodeFlow(..))


{- data Size a = Size Int Int a


data Pixels = Pixels


data Cells = Cells -}


units :: NodeFlow -> Units
units flow =
    { cell :
        { size : 40.0 <+> 40.0
        , padding : 20.0 <+> 20.0
        }
    , body :
        -- { margin : 20.0 <+> 20.0
        { size : case flow of
            Vertical -> Fixed 100.0 <+> Stretch
            Horizontal -> Stretch <+> Fixed 100.0
        , strokeWidth : 1.0
        , cornerRadius : 0.0
        , shadowShift : 5.0
        , shadowBlur : 0.0
        , margin : 50.0 <+> 10.0
        }
    , title :
        { size : case flow of
            Vertical -> Stretch <+> Fixed 20.0
            Horizontal -> Fixed 20.0 <+> Stretch
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
