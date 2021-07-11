module App.Style.Quartz
    (style) where


import App.Style (Style, Connector(..))

import App.Style.Quartz.Colors (colors)
import App.Style.Quartz.Units (units)


style :: Style
style =
    { colors
    , units
    , connector : Circle
    }