module App.Style.Quartz.Units
    (units) where


import Data.Vec2 ((<+>))

import App.Style (Units, Side(..), NodeFlow)


{- data Size a = Size Int Int a


data Pixels = Pixels


data Cells = Cells -}


units :: NodeFlow -> Units
units _ =
    { cell :
        { size : 40.0 <+> 40.0
        , padding : 20.0 <+> 20.0
        }
    , nodeBody :
        -- { margin : 20.0 <+> 20.0
        { size : Fixed 80.0 <+> Stretch
        , strokeWidth : 1.0
        , cornerRadius : 0.0
        , shadowShift : 5.0
        , shadowBlur : 0.0
        , margin : 20.0 <+> 20.0
        }
    , title :
        { size : Fixed 15.0 <+> Stretch
        , padding : 3.0 <+> 5.0
        }
    , slot :
        { radius : 3.5
        , strokeWidth : 1.5
        , outerSize : 50.0 <+> 25.0
        -- , padding
        , inletsOffset : 0.0 <+> 0.0
        , outletsOffset : 0.0 <+> 0.0
        }
    }
