module Toolkit.Hydra2.Lang.Api where

import Prelude (Unit, flip, unit, ($), (<<<))


import Toolkit.Hydra2.Types
import Toolkit.Hydra2.Types (Value(..)) as T

import Toolkit.Hydra2.Lang (Command(..), Program(..), Single(..))


unknown ∷ Program Unit -- private
unknown = Program Unknown unit


q :: Command -> Program Unit -- private
q cmd = Program cmd unit


s0 ∷ Source
s0 = S0


h0 :: AudioBin
h0 = H0


o0 ∷ Output
o0 = Output0


initCam ∷ Source → Program Unit
initCam = q <<< One <<< InitCam


initCam' ∷ Source → Value -> Program Unit
initCam' s = q <<< One <<< InitCamIdx s


src :: Source -> Texture
src = From


a :: Audio
a = Mic


show :: Audio -> Program Unit
show = q <<< One <<< WithAudio <<< Show


setBins :: Int -> Audio -> Program Unit
setBins bins = q <<< One <<< WithAudio <<< flip SetBins bins


osc ∷ Value → Value → Value → Texture
osc frequency sync offset =
    From $ Osc { frequency, sync, offset }


modulate :: Texture -> Value -> Texture -> Texture
modulate what value with =
    ModulateWith { what, with } $ Modulate value


saturate :: Value -> Texture -> Texture
saturate v =
    flip WithColor $ Saturate v


pixelate ∷ Value → Value → Texture -> Texture
pixelate pixelX pixelY =
    flip Geometry $ GPixelate { pixelX, pixelY }


scale ∷ Value → Texture → Texture
scale amount =
    flip Geometry $ GScale
        { amount
        , offsetX : Number 0.0
        , offsetY : Number 0.0
        , xMult : Number 1.0
        , yMult : Number 1.0
        }


fn :: (Context -> Value) -> Value
fn = Dep


out :: Output -> Texture -> Program Unit
out output =
    q <<< End output


fft :: AudioBin -> Audio -> Value
fft = flip Audio


render :: Output -> Program Unit
render = q <<< One <<< Render <<< Output


renderAll :: Program Unit
renderAll = q $ One $ Render All


n :: Number -> Value
n = Number