module Noodle.Ui.Cli.Palette.Set.Pico8 where

import Prelude (negate)



import Noodle.Ui.Cli.Palette.Item (Item, idxHexRgb, idx2HexRgb)



black = idxHexRgb 0 "#000000" 0 0 0 "black" :: Item
darkBlue = idxHexRgb 1 "#1D2B53" 29 43 83 "dark-blue" :: Item
darkPurple = idxHexRgb 2 "#7E2553" 126 37 83 "dark-purple" :: Item
darkGreen = idxHexRgb 3 "#008751" 0 135 81 "dark-green" :: Item
brown = idxHexRgb 4 "#AB5236" 171 82 54 "brown" :: Item
darkGrey = idxHexRgb 5 "#5F574F" 95 87 79 "dark-grey" :: Item
lightGrey = idxHexRgb 6 "#C2C3C7" 194 195 199 "light-grey" :: Item
white = idxHexRgb 7 "#FFF1E8" 255 241 232 "white" :: Item
red = idxHexRgb 8 "#FF004D" 255 0 77 "red" :: Item
orange = idxHexRgb 9 "#FFA300" 255 163 0 "orange" :: Item
yellow = idxHexRgb 10 "#FFEC27" 255 236 39 "yellow" :: Item
green = idxHexRgb 11 "#00E436" 0 228 54 "green" :: Item
blue = idxHexRgb 12 "#29ADFF" 41 173 255 "blue" :: Item
lavender = idxHexRgb 13 "#83769C" 131 118 156 "lavender" :: Item
pink = idxHexRgb 14 "#FF77A8" 255 119 168 "pink" :: Item
lightPeach = idxHexRgb 15 "#FFCCAA" 255 204 170 "light-peach" :: Item
brownishBlack = idx2HexRgb 128 (-16) "#291814" 41 24 20 "brownish-black" :: Item
darkerBlue = idx2HexRgb 129 (-15) "#111D35" 17 29 53 "darker-blue" :: Item
darkerPurple = idx2HexRgb 130 (-14) "#422136" 66 33 54 "darker-purple" :: Item
blueGreen = idx2HexRgb 131 (-13) "#125359" 18 83 89 "blue-green" :: Item
darkBrown = idx2HexRgb 132 (-12) "#742F29" 116 47 41 "dark-brown" :: Item
darkerGrey = idx2HexRgb 133 (-11) "#49333B" 73 51 59 "darker-grey" :: Item
mediumGrey = idx2HexRgb 134 (-10) "#A28879" 162 136 121 "medium-grey" :: Item
lightYellow = idx2HexRgb 135 (-9) "#F3EF7D" 243 239 125 "light-yellow" :: Item
darkRed = idx2HexRgb 136 (-8) "#BE1250" 190 18 80 "dark-red" :: Item
darkOrange = idx2HexRgb 137 (-7) "#FF6C24" 255 108 36 "dark-orange" :: Item
limeGreen = idx2HexRgb 138 (-6) "#A8E72E" 168 231 46 "lime-green" :: Item
mediumGreen = idx2HexRgb 139 (-5) "#00B543" 0 181 67 "medium-green" :: Item
trueBlue = idx2HexRgb 140 (-4) "#065AB5" 6 90 181 "true-blue" :: Item
mauve = idx2HexRgb 141 (-3) "#754665" 117 70 101 "mauve" :: Item
darkPeach = idx2HexRgb 142 (-2) "#FF6E59" 255 110 89 "dark-peach" :: Item
peach = idx2HexRgb 143 (-1) "#FF9D81" 255 157 129 "peach" :: Item




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
