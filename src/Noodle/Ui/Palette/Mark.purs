module Noodle.Ui.Palette.Mark where


import Prelude (Unit, const, ($))


import Color (Color)
import Color (rgb) as Color


class Mark a where
    mark :: a -> Color


instance Mark Unit where
    mark :: Unit -> Color
    mark = const $ Color.rgb 102 205 170