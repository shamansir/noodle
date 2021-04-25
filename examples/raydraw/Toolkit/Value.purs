module RayDraw.Toolkit.Value where


import Control.Apply
import Prelude

import Data.Array.NonEmpty (index)
import Data.Either (either)
import Data.Int (floor)
import Data.Int (toNumber, round, fromStringAs, toStringAs, hexadecimal)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String.Regex (regex, parseFlags, match)
import Text.Smolder.SVG.Attributes (color)
import Data.Number.Format(toStringWith, precision)

newtype RgbaColor = RgbaColor { r :: Number, g :: Number, b :: Number, a :: Number }
newtype ProductPalette = ProductPalette { color1 :: RgbaColor, color2 :: RgbaColor, color3 :: RgbaColor } 

productPalette :: RgbaColor -> RgbaColor -> RgbaColor -> ProductPalette
productPalette c1 c2 c3 = ProductPalette {
    color1 : c1,
    color2 : c2,
    color3 : c3    
}

getColor1 :: ProductPalette -> RgbaColor
getColor1 (ProductPalette {color1, color2, color3}) = color1 

getColor2 :: ProductPalette -> RgbaColor
getColor2 (ProductPalette {color1, color2, color3}) = color2

getColor3 :: ProductPalette -> RgbaColor
getColor3 (ProductPalette {color1, color2, color3}) = color3 

type Point2D = { x :: Number, y :: Number}
newtype RayPoints = RayPoints (Array Point2D)

rayPoints :: Array Point2D -> RayPoints
rayPoints = RayPoints

rayPointsToArray :: RayPoints -> Array (Array Number)
rayPointsToArray (RayPoints points) = 
    let
      pointToArray point2d = [point2d.x, point2d.y]       
    in points <#> pointToArray


data Value
    = Bang
    | Color RgbaColor
    | Palette ProductPalette
    | Points RayPoints

instance showValue :: Show Value where
    show Bang = "◌"
    show (Color color) = "color: " <> show color
    show (Palette palette) = "palette: " <> show palette
    show (Points points) = "points"

instance showRgbaColor :: Show RgbaColor where
    show (RgbaColor { r, g, b, a }) =
        "rgba(" <> toStringWith (precision 3) r <> ","
                <> toStringWith (precision 3) g <> ","
                <> toStringWith (precision 3) b <> ","
                <> toStringWith (precision 3) a <> ")"

instance showProductPalette :: Show ProductPalette where 
    show (ProductPalette {color1, color2, color3}) = 
        show color1 <> " ; " <> show color2 <> " ; " <> show color3

colorToCss :: RgbaColor -> String
colorToCss (RgbaColor { r, g, b, a }) =
    "rgba(" <> (show $ floor $ r * 255.0) <> ","
            <> (show $ floor $ g * 255.0) <> ","
            <> (show $ floor $ b * 255.0) <> ","
            <> show a <> ")"

hexToColor :: String -> Maybe RgbaColor
hexToColor str = do
  pattern <- hush mPattern
  groups <- match pattern str
  r <- parseHex <$> join (index groups 1)
  g <- parseHex <$> join (index groups 2)
  b <- parseHex <$> join (index groups 3)
  pure (RgbaColor {r : norm r, g : norm g, b : norm b, a: 1.0})
  where
    digit = "[0-9a-f]"
    pair = "(" <> digit <> digit <> ")"
    variant = pair <> pair <> pair
    mPattern = regex ("^#(?:" <> variant <> ")$") (parseFlags "i")
    hush = either (const Nothing) Just
    parseHex = fromMaybe 0 <<< fromStringAs hexadecimal
    norm val = (toNumber val) / 255.0

data Product
    = JetBrains
    | Space
    | IntelliJ
    | IntelliJEdu
    | PhpStorm
    | PyCharm
    | PyCharmEdu
    | RubyMine
    | WebStorm
    | CLion
    | DataGrip
    | AppCode
    | GoLand
    | ReSharper
    | ReSharperCpp
    | DotCover
    | DotMemory
    | DotPeek
    | DotTrace
    | Rider
    | TeamCity
    | YouTrack
    | Upsource
    | Hub
    | Kotlin
    | MPS
    | Mono
    | Datalore
    | DataSpell
    | Qodana
    | Toolbox

allProducts :: Array Product
allProducts = [
       JetBrains,       
       IntelliJ,    
       PhpStorm,
       PyCharm,       
       RubyMine,
       WebStorm,
       CLion,
       DataGrip,
       AppCode,
       GoLand,
       ReSharper,
       ReSharperCpp,
       DotCover,
       DotMemory,
       DotPeek,
       DotTrace,       
       TeamCity,
       YouTrack,
       Upsource,
       Hub,
       Kotlin,
       MPS
       ]


defaultColor :: RgbaColor
defaultColor = RgbaColor {r: 0.0, g : 0.0, b : 0.0, a: 0.0}

getPalette :: Product -> ProductPalette
getPalette product =
    let 
        p :: String -> String -> String -> ProductPalette 
        p a b c = productPalette (parse a) (parse b) (parse c)
        parse :: String -> RgbaColor 
        parse str = fromMaybe defaultColor (hexToColor str)
    in
    case product of        
        JetBrains ->    p "#ed3d7d" "#7c59a4" "#fcee39"
        Space ->        p "#003cb7" "#5fccf5" "#adf03e"
        IntelliJ ->     p "#007efc" "#fe315d" "#f97a12"
        PhpStorm ->     p "#b345f1" "#765af8" "#ff318c"
        PyCharm ->      p "#21d789" "#fcf84a" "#07c3f2"
        RubyMine ->     p "#fe2857" "#fc801d" "#9039d0"
        WebStorm ->     p "#07c3f2" "#087cfa" "#fcf84a"
        CLion ->        p "#21d789" "#009ae5" "#ed358c"
        DataGrip ->     p "#22d88f" "#9775f8" "#ff318c"
        AppCode ->      p "#087cfa" "#07c3f2" "#21d789"
        GoLand ->       p "#0d7bf7" "#b74af7" "#3bea62"
        ReSharper ->    p "#c21456" "#e14ce3" "#fdbc2c"
        ReSharperCpp -> p "#fdbc2c" "#e14ce3" "#c21456"
        DotCover ->     p "#ff7500" "#7866ff" "#e343e6"
        DotMemory ->    p "#ffbd00" "#7866ff" "#e343e6"
        DotPeek ->      p "#00caff" "#7866ff" "#e343e6"
        DotTrace ->     p "#fc1681" "#786bfb" "#e14ce3"
        Rider ->        p "#c90f5e" "#077cfb" "#fdb60d"
        TeamCity ->     p "#0cb0f2" "#905cfb" "#3bea62"
        Toolbox ->      p "#af1df5" "#fe2857" "#fc801d"
        YouTrack ->     p "#0cb0f2" "#905cfb" "#ff318c"
        Upsource ->     p "#22b1ef" "#9062f7" "#fd8224"
        Hub ->          p "#00b8f1" "#9758fb" "#ffee45"
        Kotlin ->       p "#627cf7" "#d44ea3" "#ff6925"
        MPS ->          p "#0b8fff" "#21d789" "#ffdc52"
        IntelliJEdu ->  p "#0d7bf7" "#fe315d" "#f97a12"
        PyCharmEdu ->   p "#21d789" "#fcf84a" "#07c3f2"
        Mono ->         p "#ffffff" "#4c4c4c" "#000000"
        Datalore ->     p "#3bea62" "#6b57ff" "#07c3f2"
        DataSpell ->    p "#087cfa" "#21d789" "#fcf84a"
        Qodana ->       p "#07c3f2" "#6b57ff" "#fa3290"        

productShortName :: Product -> String
productShortName product = 
    case product of 
        JetBrains     -> "JB"                              
        Space         -> "SP"      
        IntelliJ      -> "IJ"         
        IntelliJEdu   -> "IJe"            
        PhpStorm      -> "PS"         
        PyCharm       -> "PC"        
        PyCharmEdu    -> "PCe"           
        RubyMine      -> "RM"         
        WebStorm      -> "WS"         
        CLion         -> "CL"      
        DataGrip      -> "DG"         
        AppCode       -> "AC"        
        GoLand        -> "GL"       
        ReSharper     -> "R#"          
        ReSharperCpp  -> "R++"             
        DotCover      -> "DC"         
        DotMemory     -> "DM"          
        DotPeek       -> "DP"        
        DotTrace      -> "DT"         
        Rider         -> "RD"      
        TeamCity      -> "TC"         
        YouTrack      -> "YT"         
        Upsource      -> "US"         
        Hub           -> "HB"    
        Kotlin        -> "KT"       
        MPS           -> "MPS"    
        Mono          -> "MO"     
        Datalore      -> "DL"         
        DataSpell     -> "DS"          
        Qodana        -> "QO"       
        Toolbox       -> "TB"                    