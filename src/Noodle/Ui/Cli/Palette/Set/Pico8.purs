module Noodle.Ui.Cli.Palette.Set.Pico8 where

import Prelude (negate)



import Noodle.Ui.Cli.Palette.Item (Item, item, item')



black = item 0 "#000000" 0 0 0 "black" :: Item
darkBlue = item 1 "#1D2B53" 29 43 83 "dark-blue" :: Item
darkPurple = item 2 "#7E2553" 126 37 83 "dark-purple" :: Item
darkGreen = item 3 "#008751" 0 135 81 "dark-green" :: Item
brown = item 4 "#AB5236" 171 82 54 "brown" :: Item
darkGrey = item 5 "#5F574F" 95 87 79 "dark-grey" :: Item
lightGrey = item 6 "#C2C3C7" 194 195 199 "light-grey" :: Item
white = item 7 "#FFF1E8" 255 241 232 "white" :: Item
red = item 8 "#FF004D" 255 0 77 "red" :: Item
orange = item 9 "#FFA300" 255 163 0 "orange" :: Item
yellow = item 10 "#FFEC27" 255 236 39 "yellow" :: Item
green = item 11 "#00E436" 0 228 54 "green" :: Item
blue = item 12 "#29ADFF" 41 173 255 "blue" :: Item
lavender = item 13 "#83769C" 131 118 156 "lavender" :: Item
pink = item 14 "#FF77A8" 255 119 168 "pink" :: Item
lightPeach = item 15 "#FFCCAA" 255 204 170 "light-peach" :: Item
brownishBlack = item' 128 (-16) "#291814" 41 24 20 "brownish-black" :: Item
darkerBlue = item' 129 (-15) "#111D35" 17 29 53 "darker-blue" :: Item
darkerPurple = item' 130 (-14) "#422136" 66 33 54 "darker-purple" :: Item
blueGreen = item' 131 (-13) "#125359" 18 83 89 "blue-green" :: Item
darkBrown = item' 132 (-12) "#742F29" 116 47 41 "dark-brown" :: Item
darkerGrey = item' 133 (-11) "#49333B" 73 51 59 "darker-grey" :: Item
mediumGrey = item' 134 (-10) "#A28879" 162 136 121 "medium-grey" :: Item
lightYellow = item' 135 (-9) "#F3EF7D" 243 239 125 "light-yellow" :: Item
darkRed = item' 136 (-8) "#BE1250" 190 18 80 "dark-red" :: Item
darkOrange = item' 137 (-7) "#FF6C24" 255 108 36 "dark-orange" :: Item
limeGreen = item' 138 (-6) "#A8E72E" 168 231 46 "lime-green" :: Item
mediumGreen = item' 139 (-5) "#00B543" 0 181 67 "medium-green" :: Item
trueBlue = item' 140 (-4) "#065AB5" 6 90 181 "true-blue" :: Item
mauve = item' 141 (-3) "#754665" 117 70 101 "mauve" :: Item
darkPeach = item' 142 (-2) "#FF6E59" 255 110 89 "dark-peach" :: Item
peach = item' 143 (-1) "#FF9D81" 255 157 129 "peach" :: Item




pico8 :: Array Item
pico8 =
    [ black
    , darkBlue
    , darkPurple
    , darkGreen
    , brown
    , darkGrey
    , lightGrey
    , white
    , red
    , orange
    , yellow
    , green
    , blue
    , lavender
    , pink
    , lightPeach
    , brownishBlack
    , darkerBlue
    , darkerPurple
    , blueGreen
    , darkBrown
    , darkerGrey
    , mediumGrey
    , lightYellow
    , darkRed
    , darkOrange
    , limeGreen
    , mediumGreen
    , trueBlue
    , mauve
    , darkPeach
    , peach
    ]
