module JetBrains.Palettes where


import Prelude (($), (#), (<$>), (/), flip, (<<<))

import Data.Color as C
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Int (toNumber)
import Data.Vec (Vec, (+>), empty, (!!))
import Data.Typelevel.Num.Reps (D3, d0, d1, d2)


import Hydra
import Hydra (Color, Source) as Hydra


type PaletteId = String


type Color = Vec D3 Int


type Palette = Vec D3 Color


c :: Int -> Int -> Int -> Color
c r g b = r +> g +> b +> empty


toRegular :: Color -> C.Color
toRegular c = C.rgb (c !! d0) (c !! d1) (c !! d2)


toArray :: Palette -> Array C.Color
toArray p = toRegular <$> [ p !! d0, p !! d1, p !! d2 ]


default :: Palette
default = c 237 61 125 +> c 124 89 164 +> c 252 238 57 +> empty


palettes :: PaletteId /-> Palette
palettes =
    [ "JetBrains"    /\ (c 237 61 125  +> c 124 89 164  +> c 252 238 57  +> empty)
    , "Space"        /\ (c 0 60 183    +> c 95 204 245  +> c 173 240 62  +> empty)
    , "IntelliJ"     /\ (c 0 126 252   +> c 254 49 93   +> c 249 122 18  +> empty)
    , "PhpStorm"     /\ (c 179 69 241  +> c 118 90 248  +> c 255 49 140  +> empty)
    , "PyCharm"      /\ (c 33 215 137  +> c 252 248 74  +> c 7 195 242   +> empty)
    , "RubyMine"     /\ (c 254 40 87   +> c 252 128 29  +> c 144 57 208  +> empty)
    , "WebStorm"     /\ (c 7 195 242   +> c 8 124 250   +> c 252 248 74  +> empty)
    , "CLion"        /\ (c 33 215 137  +> c 0 154 229   +> c 237 53 140  +> empty)
    , "DataGrip"     /\ (c 34 216 143  +> c 151 117 248 +> c 255 49 140  +> empty)
    , "DataSpell"    /\ (c 8 124 250   +> c 33 215 137  +> c 252 248 74  +> empty)
    , "AppCode"      /\ (c 8 124 250   +> c 7 195 242   +> c 33 215 137  +> empty)
    , "GoLand"       /\ (c 13 123 247  +> c 183 74 247  +> c 59 234 98   +> empty)
    , "ReSharper"    /\ (c 194 20 86   +> c 225 76 227  +> c 253 188 44  +> empty)
    , "ReSharperCpp" /\ (c 253 188 44  +> c 225 76 227  +> c 194 20 86   +> empty)
    , "DotCover"     /\ (c 255 117 0   +> c 120 102 255 +> c 227 67 230  +> empty)
    , "DotMemory"    /\ (c 255 189 0   +> c 120 102 255 +> c 227 67 230  +> empty)
    , "DotPeek"      /\ (c 0 202 255   +> c 120 102 255 +> c 227 67 230  +> empty)
    , "DotTrace"     /\ (c 252 22 129  +> c 120 107 251 +> c 225 76 227  +> empty)
    , "Rider"        /\ (c 201 15 94   +> c 7 124 251   +> c 253 182 13  +> empty)
    , "TeamCity"     /\ (c 12 176 242  +> c 144 92 251  +> c 59 234 98   +> empty)
    , "YouTrack"     /\ (c 12 176 242  +> c 144 92 251  +> c 255 49 140  +> empty)
    , "Upsource"     /\ (c 34 177 239  +> c 144 98 247  +> c 253 130 36  +> empty)
    , "Hub"          /\ (c 0 184 241   +> c 151 88 251  +> c 255 238 69  +> empty)
    , "Kotlin"       /\ (c 98 124 247  +> c 212 78 163  +> c 255 105 37  +> empty)
    , "MPS"          /\ (c 11 143 255  +> c 33 215 137  +> c 255 220 82  +> empty)
    , "Mono"         /\ (c 255 255 255 +> c 76 76 76    +> c 0 0 0       +> empty)
    , "IntelliJEdu"  /\ (c 13 123 247  +> c 254 49 93   +> c 249 122 18  +> empty)
    , "PyCharmEdu"   /\ (c 33 215 137  +> c 252 248 74  +> c 7 195 242   +> empty)
    , "Datalore"     /\ (c 59 234 98   +> c 107 87 255  +> c 7 195 242   +> empty)
    ] # Map.fromFoldable


toRgbaVals :: Palette -> { r :: Value, g :: Value, b :: Value, a :: Value }
toRgbaVals p =
    { r : adapt [ (p !! d0) !! d0, (p !! d1) !! d0, (p !! d2) !! d0 ]
    , g : adapt [ (p !! d0) !! d1, (p !! d1) !! d1, (p !! d2) !! d1 ]
    , b : adapt [ (p !! d0) !! d2, (p !! d1) !! d2, (p !! d2) !! d2 ]
    , a : Seq [ Num 1.0, Num 1.0, Num 1.0 ]
    }
    where adapt arr = Seq $ Num <$> (flip (/) 255.0) <$> toNumber <$> arr


toColorMod :: Palette -> Hydra.Color
toColorMod =
    Color <<< toRgbaVals


toSolidSource :: Palette -> Hydra.Source
toSolidSource =
    Solid <<< toRgbaVals