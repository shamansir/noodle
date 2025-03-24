module Noodle.Ui.Palette.Set.Flexoki where

import Prelude


import Noodle.Ui.Palette.Item (Hex(..), Item, strHex)


base :: Theme -> Base
base = case _ of
    Dark ->
        { bg : black
        , bg_2 : base_.i950
        , ui : base_.i900
        , ui_2 : base_.i850
        , ui_3 : base_.i800
        , tx_3 : base_.i700
        , tx_2 : base_.i500
        , tx : base_.i200
        }
    Light ->
        { bg : paper
        , bg_2 : base_.i50
        , ui : base_.i100
        , ui_2 : base_.i150
        , ui_3 : base_.i200
        , tx_3 : base_.i300
        , tx_2 : base_.i600
        , tx : black
        }


accent :: Theme -> Accent
accent = case _ of
    Dark ->
        { re : red.i400
        , or : orange.i400
        , ye : yellow.i400
        , gr : green.i400
        , cy : cyan.i400
        , bl : blue.i400
        , pu : purple.i400
        , ma : magenta.i400
        , re_2 : red.i600
        , or_2 : orange.i600
        , ye_2 : yellow.i600
        , gr_2 : green.i600
        , cy_2 : cyan.i600
        , bl_2 : blue.i600
        , pu_2 : purple.i600
        , ma_2 : magenta.i600
        }

    Light ->
        { re : red.i600
        , or : orange.i600
        , ye : yellow.i600
        , gr : green.i600
        , cy : cyan.i600
        , bl : blue.i600
        , pu : purple.i600
        , ma : magenta.i600
        , re_2 : red.i400
        , or_2 : orange.i400
        , ye_2 : yellow.i400
        , gr_2 : green.i400
        , cy_2 : cyan.i400
        , bl_2 : blue.i400
        , pu_2 : purple.i400
        , ma_2 : magenta.i400
        }





data Theme
    = Dark
    | Light


type Base =
    { bg :: Item
    , bg_2 :: Item
    , ui :: Item
    , ui_2 :: Item
    , ui_3 :: Item
    , tx_3 :: Item
    , tx_2 :: Item
    , tx :: Item
    }


type Accent =
    { re :: Item
    , or :: Item
    , ye :: Item
    , gr :: Item
    , cy :: Item
    , bl :: Item
    , pu :: Item
    , ma :: Item
    , re_2 :: Item
    , or_2 :: Item
    , ye_2 :: Item
    , gr_2 :: Item
    , cy_2 :: Item
    , bl_2 :: Item
    , pu_2 :: Item
    , ma_2 :: Item
    }


type Ladder =
    { i50 :: Item
    , i100 :: Item
    , i150 :: Item
    , i200 :: Item
    , i300 :: Item
    , i400 :: Item
    , i500 :: Item
    , i600 :: Item
    , i700 :: Item
    , i800 :: Item
    , i850 :: Item
    , i900 :: Item
    , i950 :: Item
    }



black :: Item
black = strHex "black" $ Hex "100F0F"


paper :: Item
paper = strHex "paper" $ Hex "FFFCF0"


ladderToItems :: Ladder -> Array Item
ladderToItems set  =
    [ set.i50
    , set.i100
    , set.i150
    , set.i200
    , set.i300
    , set.i400
    , set.i500
    , set.i600
    , set.i700
    , set.i800
    , set.i850
    , set.i900
    , set.i950
    ]


accentToItems :: Accent -> Array Item
accentToItems accent =
    [ accent.re
    , accent.or
    , accent.ye
    , accent.gr
    , accent.cy
    , accent.bl
    , accent.pu
    , accent.ma
    , accent.re_2
    , accent.or_2
    , accent.ye_2
    , accent.gr_2
    , accent.cy_2
    , accent.bl_2
    , accent.pu_2
    , accent.ma_2
    ]


baseToItems :: Base -> Array Item
baseToItems base =
    [ base.bg
    , base.bg_2
    , base.ui
    , base.ui_2
    , base.ui_3
    , base.tx_3
    , base.tx_2
    , base.tx
    ]



base_ :: Ladder
base_ =
    { i50 : strHex "base-50" $ Hex "F2F0E5"
    , i100 : strHex "base-100" $ Hex "E6E4D9"
    , i150 : strHex "base-150" $ Hex "DAD8CE"
    , i200 : strHex "base-200" $ Hex "CECDC3"
    , i300 : strHex "base-300" $ Hex "B7B5AC"
    , i400 : strHex "base-400" $ Hex "9F9D96"
    , i500 : strHex "base-500" $ Hex "878580"
    , i600 : strHex "base-600" $ Hex "6F6E69"
    , i700 : strHex "base-700" $ Hex "575653"
    , i800 : strHex "base-800" $ Hex "403E3C"
    , i850 : strHex "base-850" $ Hex "343331"
    , i900 : strHex "base-900" $ Hex "282726"
    , i950 : strHex "base-950" $ Hex "1C1B1A"
    }

red :: Ladder
red =
    { i50 : strHex "red-50" $ Hex "FFE1D5"
    , i100 : strHex "red-100" $ Hex "FFCABB"
    , i150 : strHex "red-150" $ Hex "FDB2A2"
    , i200 : strHex "red-200" $ Hex "F89A8A"
    , i300 : strHex "red-300" $ Hex "E8705F"
    , i400 : strHex "red-400" $ Hex "D14D41"
    , i500 : strHex "red-500" $ Hex "C03E35"
    , i600 : strHex "red-600" $ Hex "AF3029"
    , i700 : strHex "red-700" $ Hex "942822"
    , i800 : strHex "red-800" $ Hex "6C201C"
    , i850 : strHex "red-850" $ Hex "551B18"
    , i900 : strHex "red-900" $ Hex "3E1715"
    , i950 : strHex "red-950" $ Hex "261312"
    }

orange :: Ladder
orange =
    { i50 : strHex "orange-50" $ Hex "FFE7CE"
    , i100 : strHex "orange-100" $ Hex "FED3AF"
    , i150 : strHex "orange-150" $ Hex "FCC192"
    , i200 : strHex "orange-200" $ Hex "F9AE77"
    , i300 : strHex "orange-300" $ Hex "EC8B49"
    , i400 : strHex "orange-400" $ Hex "DA702C"
    , i500 : strHex "orange-500" $ Hex "CB6120"
    , i600 : strHex "orange-600" $ Hex "BC5215"
    , i700 : strHex "orange-700" $ Hex "9D4310"
    , i800 : strHex "orange-800" $ Hex "71320D"
    , i850 : strHex "orange-850" $ Hex "59290D"
    , i900 : strHex "orange-900" $ Hex "40200D"
    , i950 : strHex "orange-950" $ Hex "27180E"
    }

yellow :: Ladder
yellow =
    { i50 : strHex "yellow-50" $ Hex "FAEEC6"
    , i100 : strHex "yellow-100" $ Hex "F6E2A0"
    , i150 : strHex "yellow-150" $ Hex "F1D67E"
    , i200 : strHex "yellow-200" $ Hex "ECCB60"
    , i300 : strHex "yellow-300" $ Hex "DFB431"
    , i400 : strHex "yellow-400" $ Hex "D0A215"
    , i500 : strHex "yellow-500" $ Hex "BE9207"
    , i600 : strHex "yellow-600" $ Hex "AD8301"
    , i700 : strHex "yellow-700" $ Hex "8E6B01"
    , i800 : strHex "yellow-800" $ Hex "664D01"
    , i850 : strHex "yellow-850" $ Hex "503D02"
    , i900 : strHex "yellow-900" $ Hex "3A2D04"
    , i950 : strHex "yellow-950" $ Hex "241E08"
    }


green :: Ladder
green =
    { i50 : strHex "green-50" $ Hex "EDEECF"
    , i100 : strHex "green-100" $ Hex "DDE2B2"
    , i150 : strHex "green-150" $ Hex "CDD597"
    , i200 : strHex "green-200" $ Hex "BEC97E"
    , i300 : strHex "green-300" $ Hex "A0AF54"
    , i400 : strHex "green-400" $ Hex "879A39"
    , i500 : strHex "green-500" $ Hex "768D21"
    , i600 : strHex "green-600" $ Hex "66800B"
    , i700 : strHex "green-700" $ Hex "536907"
    , i800 : strHex "green-800" $ Hex "3D4C07"
    , i850 : strHex "green-850" $ Hex "313D07"
    , i900 : strHex "green-900" $ Hex "252D09"
    , i950 : strHex "green-950" $ Hex "1A1E0C"
    }


cyan :: Ladder
cyan =
    { i50 : strHex "cyan-50" $ Hex "DDF1E4"
    , i100 : strHex "cyan-100" $ Hex "BFE8D9"
    , i150 : strHex "cyan-150" $ Hex "A2DECE"
    , i200 : strHex "cyan-200" $ Hex "87D3C3"
    , i300 : strHex "cyan-300" $ Hex "5ABDAC"
    , i400 : strHex "cyan-400" $ Hex "3AA99F"
    , i500 : strHex "cyan-500" $ Hex "2F968D"
    , i600 : strHex "cyan-600" $ Hex "24837B"
    , i700 : strHex "cyan-700" $ Hex "1C6C66"
    , i800 : strHex "cyan-800" $ Hex "164F4A"
    , i850 : strHex "cyan-850" $ Hex "143F3C"
    , i900 : strHex "cyan-900" $ Hex "122F2C"
    , i950 : strHex "cyan-950" $ Hex "101F1D"
    }


blue  :: Ladder
blue =
    { i50 : strHex "blue-50" $ Hex "E1ECEB"
    , i100 : strHex "blue-100" $ Hex "C6DDE8"
    , i150 : strHex "blue-150" $ Hex "ABCFE2"
    , i200 : strHex "blue-200" $ Hex "92BFDB"
    , i300 : strHex "blue-300" $ Hex "66A0C8"
    , i400 : strHex "blue-400" $ Hex "4385BE"
    , i500 : strHex "blue-500" $ Hex "3171B2"
    , i600 : strHex "blue-600" $ Hex "205EA6"
    , i700 : strHex "blue-700" $ Hex "1A4F8C"
    , i800 : strHex "blue-800" $ Hex "163B66"
    , i850 : strHex "blue-850" $ Hex "133051"
    , i900 : strHex "blue-900" $ Hex "12253B"
    , i950 : strHex "blue-950" $ Hex "101A24"
    }


purple :: Ladder
purple =
    { i50 : strHex "purple-50" $ Hex "F0EAEC"
    , i100 : strHex "purple-100" $ Hex "E2D9E9"
    , i150 : strHex "purple-150" $ Hex "D3CAE6"
    , i200 : strHex "purple-200" $ Hex "C4B9E0"
    , i300 : strHex "purple-300" $ Hex "A699D0"
    , i400 : strHex "purple-400" $ Hex "8B7EC8"
    , i500 : strHex "purple-500" $ Hex "735EB5"
    , i600 : strHex "purple-600" $ Hex "5E409D"
    , i700 : strHex "purple-700" $ Hex "4F3685"
    , i800 : strHex "purple-800" $ Hex "3C2A62"
    , i850 : strHex "purple-850" $ Hex "31234E"
    , i900 : strHex "purple-900" $ Hex "261C39"
    , i950 : strHex "purple-950" $ Hex "1A1623"
    }


magenta :: Ladder
magenta =
    { i50 : strHex "magenta-50" $ Hex "FEE4E5"
    , i100 : strHex "magenta-100" $ Hex "FCCFDA"
    , i150 : strHex "magenta-150" $ Hex "F9B9CF"
    , i200 : strHex "magenta-200" $ Hex "F4A4C2"
    , i300 : strHex "magenta-300" $ Hex "E47DA8"
    , i400 : strHex "magenta-400" $ Hex "CE5D97"
    , i500 : strHex "magenta-500" $ Hex "B74583"
    , i600 : strHex "magenta-600" $ Hex "A02F6F"
    , i700 : strHex "magenta-700" $ Hex "87285E"
    , i800 : strHex "magenta-800" $ Hex "641F46"
    , i850 : strHex "magenta-850" $ Hex "4F1B39"
    , i900 : strHex "magenta-900" $ Hex "39172B"
    , i950 : strHex "magenta-950" $ Hex "24131D"
    }