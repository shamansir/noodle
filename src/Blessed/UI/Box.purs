module Blessed.UI.Box where


import Prelude (identity)

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dim
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Style (Style)
import Blessed.Core.Style as Style


type OptionsRow r =
    ( top :: Offset
    , left :: Offset
    , width :: Dimension
    , height :: Dimension
    , content :: String -- a ?
    , tags :: Boolean
    , draggable :: Boolean
    , hover :: (Style -> Style)
    , style :: Style
    | r
    )
type Options = Record (OptionsRow ())


default :: Options
default =
    { top : Offset.px 0
    , left : Offset.px 0
    , width : Dim.percents 100.0
    , height : Dim.percents 100.0
    , content : ""
    , tags : false
    , draggable : false
    , hover : identity
    , style : Style.default
    }