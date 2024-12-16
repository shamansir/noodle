module Noodle.Ui.Cli.Palette.Set.Catpuccin where

import Prelude

import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Ui.Cli.Palette.Item (Item, hexRgbHsl) as P


-- data FlavorK
--     = Latte
--     | Frappe
--     | Macciato
--     | Mocha


collectFlavor :: Flavor -> Array P.Item
collectFlavor r =
    [ r.rosewater
    , r.flamingo
    , r.pink
    , r.mauve
    , r.red
    , r.maroon
    , r.peach
    , r.yellow
    , r.green
    , r.teal
    , r.sky
    , r.sapphire
    , r.blue
    , r.lavender
    , r.text
    , r.subtext1
    , r.subtext0
    , r.overlay2
    , r.overlay1
    , r.overlay0
    , r.surface2
    , r.surface1
    , r.surface0
    , r.base
    , r.mantle
    , r.crust
    ]


type Flavor =
    { rosewater :: P.Item
    , flamingo :: P.Item
    , pink :: P.Item
    , mauve :: P.Item
    , red :: P.Item
    , maroon :: P.Item
    , peach :: P.Item
    , yellow :: P.Item
    , green :: P.Item
    , teal :: P.Item
    , sky :: P.Item
    , sapphire :: P.Item
    , blue :: P.Item
    , lavender :: P.Item
    , text :: P.Item
    , subtext1 :: P.Item
    , subtext0 :: P.Item
    , overlay2 :: P.Item
    , overlay1 :: P.Item
    , overlay0 :: P.Item
    , surface2 :: P.Item
    , surface1 :: P.Item
    , surface0 :: P.Item
    , base :: P.Item
    , mantle :: P.Item
    , crust :: P.Item
    }


{-
type CPItem =
    { hex :: String
    , rgb :: { r :: Int, g :: Int, b :: Int }
    , hsl :: { h :: Int, s :: Int, l :: Int }
    }
-}


q :: String -> Int -> Int -> Int -> Int -> Int -> Int -> String -> P.Item
q hex = P.hexRgbHsl $ "#" <> hex


latte :: Flavor
latte =
    { "rosewater" : q "dc8a78"   220 138 120    11 59 67 "latte-rosewater"
    , "flamingo"  : q "dd7878"   221 120 120     0 60 67 "latte-flamingo"
    , "pink"      : q "ea76cb"   234 118 203   316 73 69 "latte-pink"
    , "mauve"     : q "8839ef"   136  57 239   266 85 58 "latte-mauve"
    , "red"       : q "d20f39"   210  15  57   347 87 44 "latte-red"
    , "maroon"    : q "e64553"   230  69  83   355 76 59 "latte-maroon"
    , "peach"     : q "fe640b"   254 100  11    22 99 52 "latte-peach"
    , "yellow"    : q "df8e1d"   223 142  29    35 77 49 "latte-yellow"
    , "green"     : q "40a02b"    64 160  43   109 58 40 "latte-green"
    , "teal"      : q "179299"    23 146 153   183 74 35 "latte-teal"
    , "sky"       : q "04a5e5"     4 165 229   197 97 46 "latte-sky"
    , "sapphire"  : q "209fb5"    32 159 181   189 70 42 "latte-sapphire"
    , "blue"      : q "1e66f5"    30 102 245   220 91 54 "latte-blue"
    , "lavender"  : q "7287fd"   114 135 253   231 97 72 "latte-lavender"

    , "text"      : q "4c4f69"   76 79 105     234 16 35 "latte-text"
    , "subtext1"  : q "5c5f77"   92 95 119     233 13 41 "latte-subtext1"
    , "subtext0"  : q "6c6f85"   108 111 133   233 10 47 "latte-subtext0"

    , "overlay2"  : q "7c7f93"   124 127 147   232 10 53 "latte-overlay2"
    , "overlay1"  : q "8c8fa1"   140 143 161   231 10 59 "latte-overlay1"
    , "overlay0"  : q "9ca0b0"   156 160 176   228 11 65 "latte-overlay0"

    , "surface2"  : q "acb0be"   172 176 190   227 12 71 "latte-surface2"
    , "surface1"  : q "bcc0cc"   188 192 204   225 14 77 "latte-surface1"
    , "surface0"  : q "ccd0da"   204 208 218   223 16 83 "latte-surface0"

    , "base"      : q "eff1f5"   239 241 245   220 23 95 "latte-base"
    , "mantle"    : q "e6e9ef"   230 233 239   220 22 92 "latte-mantle"
    , "crust"     : q "dce0e8"   220 224 232   220 21 89 "latte-crust"
    }


frappe :: Flavor
frappe =
    { "rosewater" : q "f2d5cf"    242 213 207    10 57 88 "frappe-rosewater"
    , "flamingo"  : q "eebebe"    238 190 190     0 59 84 "frappe-flamingo"
    , "pink"      : q "f4b8e4"    244 184 228   316 73 84 "frappe-pink"
    , "mauve"     : q "ca9ee6"    202 158 230   277 59 76 "frappe-mauve"
    , "red"       : q "e78284"    231 130 132   359 68 71 "frappe-red"
    , "maroon"    : q "ea999c"    234 153 156   358 66 76 "frappe-maroon"
    , "peach"     : q "ef9f76"    239 159 118    20 79 70 "frappe-peach"
    , "yellow"    : q "e5c890"    229 200 144    40 62 73 "frappe-yellow"
    , "green"     : q "a6d189"    166 209 137    96 44 68 "frappe-green"
    , "teal"      : q "81c8be"    129 200 190   172 39 65 "frappe-teal"
    , "sky"       : q "99d1db"    153 209 219   189 48 73 "frappe-sky"
    , "sapphire"  : q "85c1dc"    133 193 220   199 55 69 "frappe-sapphire"
    , "blue"      : q "8caaee"    140 170 238   222 74 74 "frappe-blue"
    , "lavender"  : q "babbf1"    186 187 241   239 66 84 "frappe-lavender"

    , "text"      : q "c6d0f5"    198 208 245   227 70 87 "frappe-text"
    , "subtext1"  : q "b5bfe2"    181 191 226   227 44 80 "frappe-subtext1"
    , "subtext0"  : q "a5adce"    165 173 206   228 29 73 "frappe-subtext0"

    , "overlay2"  : q "949cbb"    148 156 187   228 22 66 "frappe-overlay2"
    , "overlay1"  : q "838ba7"    131 139 167   227 17 58 "frappe-overlay1"
    , "overlay0"  : q "737994"    115 121 148   229 13 52 "frappe-overlay0"

    , "surface2"  : q "626880"     98 104 128   228 13 44 "frappe-surface2"
    , "surface1"  : q "51576d"     81  87 109   227 15 37 "frappe-surface1"
    , "surface0"  : q "414559"     65  69  89   230 16 30 "frappe-surface0"

    , "base"      : q "303446"     48  52  70   229 19 23 "frappe-base"
    , "mantle"    : q "292c3c"     41  44  60   231 19 20 "frappe-mantle"
    , "crust"     : q "232634"     35  38  52   229 20 17 "frappe-crust"
    }


macchiato :: Flavor
macchiato =
    { "rosewater" : q "f4dbd6"    244 219 214    10 58 90 "macchiato-rosewater"
    , "flamingo"  : q "f0c6c6"    240 198 198     0 58 86 "macchiato-flamingo"
    , "pink"      : q "f5bde6"    245 189 230   316 74 85 "macchiato-pink"
    , "mauve"     : q "c6a0f6"    198 160 246   267 83 80 "macchiato-mauve"
    , "red"       : q "ed8796"    237 135 150   351 74 73 "macchiato-red"
    , "maroon"    : q "ee99a0"    238 153 160   355 71 77 "macchiato-maroon"
    , "peach"     : q "f5a97f"    245 169 127    21 86 73 "macchiato-peach"
    , "yellow"    : q "eed49f"    238 212 159    40 70 78 "macchiato-yellow"
    , "green"     : q "a6da95"    166 218 149   105 48 72 "macchiato-green"
    , "teal"      : q "8bd5ca"    139 213 202   171 47 69 "macchiato-teal"
    , "sky"       : q "91d7e3"    145 215 227   189 59 73 "macchiato-sky"
    , "sapphire"  : q "7dc4e4"    125 196 228   199 66 69 "macchiato-sapphire"
    , "blue"      : q "8aadf4"    138 173 244   220 83 75 "macchiato-blue"
    , "lavender"  : q "b7bdf8"    183 189 248   234 82 85 "macchiato-lavender"

    , "text"      : q "cad3f5"    202 211 245   227 68 88 "macchiato-text"
    , "subtext1"  : q "b8c0e0"    184 192 224   228 39 80 "macchiato-subtext1"
    , "subtext0"  : q "a5adcb"    165 173 203   227 27 72 "macchiato-subtext0"

    , "overlay2"  : q "939ab7"    147 154 183   228 20 65 "macchiato-overlay2"
    , "overlay1"  : q "8087a2"    128 135 162   228 15 57 "macchiato-overlay1"
    , "overlay0"  : q "6e738d"    110 115 141   230 12 49 "macchiato-overlay0"

    , "surface2"  : q "5b6078"     91  96 120   230 14 41 "macchiato-surface2"
    , "surface1"  : q "494d64"     73  77 100   231 16 34 "macchiato-surface1"
    , "surface0"  : q "363a4f"     54  58  79   230 19 26 "macchiato-surface0"

    , "base"      : q "24273a"     36  39  58   232 23 18 "macchiato-base"
    , "mantle"    : q "1e2030"     30  32  48   233 23 15 "macchiato-mantle"
    , "crust"     : q "181926"     24  25  38   236 23 12 "macchiato-crust"
    }


mocha :: Flavor
mocha =
    { "rosewater" : q "f5e0dc"    245 224 220    10 56 91 "mocha-rosewater"
    , "flamingo"  : q "f2cdcd"    242 205 205     0 59 88 "mocha-flamingo"
    , "pink"      : q "f5c2e7"    245 194 231   316 72 86 "mocha-pink"
    , "mauve"     : q "cba6f7"    203 166 247   267 84 81 "mocha-mauve"
    , "red"       : q "f38ba8"    243 139 168   343 81 75 "mocha-red"
    , "maroon"    : q "eba0ac"    235 160 172   350 65 77 "mocha-maroon"
    , "peach"     : q "fab387"    250 179 135    23 92 75 "mocha-peach"
    , "yellow"    : q "f9e2af"    249 226 175    41 86 83 "mocha-yellow"
    , "green"     : q "a6e3a1"    166 227 161   115 54 76 "mocha-green"
    , "teal"      : q "94e2d5"    148 226 213   170 57 73 "mocha-teal"
    , "sky"       : q "89dceb"    137 220 235   189 71 73 "mocha-sky"
    , "sapphire"  : q "74c7ec"    116 199 236   199 76 69 "mocha-sapphire"
    , "blue"      : q "89b4fa"    137 180 250   217 92 76 "mocha-blue"
    , "lavender"  : q "b4befe"    180 190 254   232 97 85 "mocha-lavender"

    , "text"      : q "cdd6f4"    205 214 244   226 64 88 "mocha-text"
    , "subtext1"  : q "bac2de"    186 194 222   227 35 80 "mocha-subtext1"
    , "subtext0"  : q "a6adc8"    166 173 200   228 24 72 "mocha-subtext0"

    , "overlay2"  : q "9399b2"    147 153 178   228 17 64 "mocha-overlay2"
    , "overlay1"  : q "7f849c"    127 132 156   230 13 55 "mocha-overlay1"
    , "overlay0"  : q "6c7086"    108 112 134   231 11 47 "mocha-overlay0"

    , "surface2"  : q "585b70"     88  91 112   233 12 39 "mocha-surface2"
    , "surface1"  : q "45475a"     69  71  90   234 13 31 "mocha-surface1"
    , "surface0"  : q "313244"     49  50  68   237 16 23 "mocha-surface0"

    , "base"      : q "1e1e2e"     30  30  46   240 21 15 "mocha-base"
    , "mantle"    : q "181825"     24  24  37   240 21 12 "mocha-mantle"
    , "crust"     : q "11111b"     17  17  27   240 23  9 "mocha-crust"
    }



catpuccin ::
    { latte :: Flavor
    , frappe :: Flavor
    , macchiato :: Flavor
    , mocha :: Flavor
    }
catpuccin =
    { latte, frappe, macchiato, mocha }


catpuccinAll :: Array P.Item
catpuccinAll
    =  collectFlavor latte
    <> collectFlavor frappe
    <> collectFlavor macchiato
    <> collectFlavor mocha