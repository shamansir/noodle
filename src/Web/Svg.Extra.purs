module Web.Svg.Extra where


import Data.Vec2 (Vec2)
import Data.Vec2 as V2

import Halogen.Svg.Attributes as HSA
import Halogen.HTML.Properties as HP


translateTo :: Vec2 -> HSA.Transform
translateTo vec = HSA.Translate (V2.x vec) (V2.y vec)


translateTo' :: forall r i. Vec2 -> HP.IProp (transform :: String | r) i
translateTo' vec = HSA.transform [ translateTo vec ]