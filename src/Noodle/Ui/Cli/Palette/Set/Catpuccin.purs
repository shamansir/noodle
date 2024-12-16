module Catpuccin where

import Prelude


data FlavorK
    = Latte
    | Frappe
    | Macciato
    | Mocha


collectFlavor :: Flavor -> Array Item
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
    { rosewater :: Item
    , flamingo :: Item
    , pink :: Item
    , mauve :: Item
    , red :: Item
    , maroon :: Item
    , peach :: Item
    , yellow :: Item
    , green :: Item
    , teal :: Item
    , sky :: Item
    , sapphire :: Item
    , blue :: Item
    , lavender :: Item
    , text :: Item
    , subtext1 :: Item
    , subtext0 :: Item
    , overlay2 :: Item
    , overlay1 :: Item
    , overlay0 :: Item
    , surface2 :: Item
    , surface1 :: Item
    , surface0 :: Item
    , base :: Item
    , mantle :: Item
    , crust :: Item
    }


type Item =
    { hex :: String
    , rgb :: { r :: Int, g :: Int, b :: Int }
    , hsl :: { h :: Int, s :: Int, l :: Int }
    }


q hex r g b h s l = { hex, rgb : { r, g, b }, hsl : { h, s, l } }


latte :: Flavor
latte =
    { "rosewater" : q "dc8a78"   220 138 120    11 59 67
    , "flamingo"  : q "dd7878"   221 120 120     0 60 67
    , "pink"      : q "ea76cb"   234 118 203   316 73 69
    , "mauve"     : q "8839ef"   136  57 239   266 85 58
    , "red"       : q "d20f39"   210  15  57   347 87 44
    , "maroon"    : q "e64553"   230  69  83   355 76 59
    , "peach"     : q "fe640b"   254 100  11    22 99 52
    , "yellow"    : q "df8e1d"   223 142  29    35 77 49
    , "green"     : q "40a02b"    64 160  43   109 58 40
    , "teal"      : q "179299"    23 146 153   183 74 35
    , "sky"       : q "04a5e5"     4 165 229   197 97 46
    , "sapphire"  : q "209fb5"    32 159 181   189 70 42
    , "blue"      : q "1e66f5"    30 102 245   220 91 54
    , "lavender"  : q "7287fd"   114 135 253   231 97 72

    , "text"      : q "4c4f69"   76 79 105     234 16 35
    , "subtext1"  : q "5c5f77"   92 95 119     233 13 41
    , "subtext0"  : q "6c6f85"   108 111 133   233 10 47

    , "overlay2"  : q "7c7f93"   124 127 147   232 10 53
    , "overlay1"  : q "8c8fa1"   140 143 161   231 10 59
    , "overlay0"  : q "9ca0b0"   156 160 176   228 11 65

    , "surface2"  : q "acb0be"   172 176 190   227 12 71
    , "surface1"  : q "bcc0cc"   188 192 204   225 14 77
    , "surface0"  : q "ccd0da"   204 208 218   223 16 83

    , "base"      : q "eff1f5"   239 241 245   220 23 95
    , "mantle"    : q "e6e9ef"   230 233 239   220 22 92
    , "crust"     : q "dce0e8"   220 224 232   220 21 89
    }


frappe :: Flavor
frappe =
    { "rosewater" : q "f2d5cf"    242 213 207    10 57 88
    , "flamingo"  : q "eebebe"    238 190 190     0 59 84
    , "pink"      : q "f4b8e4"    244 184 228   316 73 84
    , "mauve"     : q "ca9ee6"    202 158 230   277 59 76
    , "red"       : q "e78284"    231 130 132   359 68 71
    , "maroon"    : q "ea999c"    234 153 156   358 66 76
    , "peach"     : q "ef9f76"    239 159 118    20 79 70
    , "yellow"    : q "e5c890"    229 200 144    40 62 73
    , "green"     : q "a6d189"    166 209 137    96 44 68
    , "teal"      : q "81c8be"    129 200 190   172 39 65
    , "sky"       : q "99d1db"    153 209 219   189 48 73
    , "sapphire"  : q "85c1dc"    133 193 220   199 55 69
    , "blue"      : q "8caaee"    140 170 238   222 74 74
    , "lavender"  : q "babbf1"    186 187 241   239 66 84

    , "text"      : q "c6d0f5"    198 208 245   227 70 87
    , "subtext1"  : q "b5bfe2"    181 191 226   227 44 80
    , "subtext0"  : q "a5adce"    165 173 206   228 29 73

    , "overlay2"  : q "949cbb"    148 156 187   228 22 66
    , "overlay1"  : q "838ba7"    131 139 167   227 17 58
    , "overlay0"  : q "737994"    115 121 148   229 13 52

    , "surface2"  : q "626880"     98 104 128   228 13 44
    , "surface1"  : q "51576d"     81  87 109   227 15 37
    , "surface0"  : q "414559"     65  69  89   230 16 30

    , "base"      : q "303446"     48  52  70   229 19 23
    , "mantle"    : q "292c3c"     41  44  60   231 19 20
    , "crust"     : q "232634"     35  38  52   229 20 17
    }


macchiato :: Flavor
macchiato =
    { "rosewater" : q "f4dbd6"    244 219 214    10 58 90
    , "flamingo"  : q "f0c6c6"    240 198 198     0 58 86
    , "pink"      : q "f5bde6"    245 189 230   316 74 85
    , "mauve"     : q "c6a0f6"    198 160 246   267 83 80
    , "red"       : q "ed8796"    237 135 150   351 74 73
    , "maroon"    : q "ee99a0"    238 153 160   355 71 77
    , "peach"     : q "f5a97f"    245 169 127    21 86 73
    , "yellow"    : q "eed49f"    238 212 159    40 70 78
    , "green"     : q "a6da95"    166 218 149   105 48 72
    , "teal"      : q "8bd5ca"    139 213 202   171 47 69
    , "sky"       : q "91d7e3"    145 215 227   189 59 73
    , "sapphire"  : q "7dc4e4"    125 196 228   199 66 69
    , "blue"      : q "8aadf4"    138 173 244   220 83 75
    , "lavender"  : q "b7bdf8"    183 189 248   234 82 85

    , "text"      : q "cad3f5"    202 211 245   227 68 88
    , "subtext1"  : q "b8c0e0"    184 192 224   228 39 80
    , "subtext0"  : q "a5adcb"    165 173 203   227 27 72

    , "overlay2"  : q "939ab7"    147 154 183   228 20 65
    , "overlay1"  : q "8087a2"    128 135 162   228 15 57
    , "overlay0"  : q "6e738d"    110 115 141   230 12 49

    , "surface2"  : q "5b6078"     91  96 120   230 14 41
    , "surface1"  : q "494d64"     73  77 100   231 16 34
    , "surface0"  : q "363a4f"     54  58  79   230 19 26

    , "base"      : q "24273a"     36  39  58   232 23 18
    , "mantle"    : q "1e2030"     30  32  48   233 23 15
    , "crust"     : q "181926"     24  25  38   236 23 12
    }


mocha :: Flavor
mocha =
    { "rosewater" : q "f5e0dc"    245 224 220    10 56 91
    , "flamingo"  : q "f2cdcd"    242 205 205     0 59 88
    , "pink"      : q "f5c2e7"    245 194 231   316 72 86
    , "mauve"     : q "cba6f7"    203 166 247   267 84 81
    , "red"       : q "f38ba8"    243 139 168   343 81 75
    , "maroon"    : q "eba0ac"    235 160 172   350 65 77
    , "peach"     : q "fab387"    250 179 135    23 92 75
    , "yellow"    : q "f9e2af"    249 226 175    41 86 83
    , "green"     : q "a6e3a1"    166 227 161   115 54 76
    , "teal"      : q "94e2d5"    148 226 213   170 57 73
    , "sky"       : q "89dceb"    137 220 235   189 71 73
    , "sapphire"  : q "74c7ec"    116 199 236   199 76 69
    , "blue"      : q "89b4fa"    137 180 250   217 92 76
    , "lavender"  : q "b4befe"    180 190 254   232 97 85

    , "text"      : q "cdd6f4"    205 214 244   226 64 88
    , "subtext1"  : q "bac2de"    186 194 222   227 35 80
    , "subtext0"  : q "a6adc8"    166 173 200   228 24 72

    , "overlay2"  : q "9399b2"    147 153 178   228 17 64
    , "overlay1"  : q "7f849c"    127 132 156   230 13 55
    , "overlay0"  : q "6c7086"    108 112 134   231 11 47

    , "surface2"  : q "585b70"     88  91 112   233 12 39
    , "surface1"  : q "45475a"     69  71  90   234 13 31
    , "surface0"  : q "313244"     49  50  68   237 16 23

    , "base"      : q "1e1e2e"     30  30  46   240 21 15
    , "mantle"    : q "181825"     24  24  37   240 21 12
    , "crust"     : q "11111b"     17  17  27   240 23  9
    }



-- (\w+)\s+#([\w\d]{6})\s+rgb\((\d+),\s(\d+),\s(\d+)\)\s+hsl\((\d+),\s(\d+)%,\s(\d+)%\)
--     , "\L$1"  : q "$2"    $3 $4 $5   $6 $7 $8





{-
type Item =
    { kind :: }
-}