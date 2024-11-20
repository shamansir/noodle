module Noodle.Ui.Cli.Palette.Mark where


import Prelude (Unit, const, ($))


import Color (Color)
import Color (rgb) as Color


import Noodle.Id (FamilyR)


class Mark a where
    mark :: a -> Color


instance Mark Unit where
    mark :: Unit -> Color
    mark = const $ Color.rgb 102 205 170


instance Mark FamilyR where  -- FIXME: Remove
    mark = const $ Color.rgb 102 205 170