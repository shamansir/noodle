module Toolkit.Hydra2.Lang.Glsl where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))


import Toolkit.Hydra2.Types as H
import Toolkit.Hydra2.Lang.Fn as Fn


-- examples are from: https://hydra-book.glitch.me/#/glsl


choma :: H.GlslFn
choma =
    H.GlslFn
    $ H.FnColor /\
    """float maxrb = max( _c0.r, _c0.b );
   float k = clamp( (_c0.g-maxrb)*5.0, 0.0, 1.0 );
   float dg = _c0.g;
   _c0.g = min( _c0.g, maxrb*0.8 );
   _c0 += vec4(dg - _c0.g);
   return vec4(_c0.rgb, 1.0 - k);
   """
   /\ (Fn.Fn $ "color" /\
            [ Fn.Argument "radius" $ H.V $ H.Number 4.0
            , Fn.Argument "rot" $ H.V $ H.Number 0.0
            ]
       )


sphere :: H.GlslFn
sphere =
    H.GlslFn
    $ H.FnCoord /\
    """vec2 pos = _st-0.5;
  vec3 rpos = vec3(0.0, 0.0, -10.0);
  vec3 rdir = normalize(vec3(pos * 3.0, 1.0));
  float d = 0.0;
  for(int i = 0; i < 16; ++i){
    d = length(rpos) - radius;
    rpos += d * rdir;
    if (abs(d) < 0.001)break;
  }
  return vec2(atan(rpos.z, rpos.x)+rot, atan(length(rpos.xz), rpos.y));
    """
       /\ (Fn.Fn $ "sphere" /\
            [ Fn.Argument "radius" $ H.V $ H.Number 4.0
            , Fn.Argument "rot" $ H.V $ H.Number 0.0
            ]
       )


sphereDisplacement :: H.GlslFn
sphereDisplacement =
    H.GlslFn
    $ H.FnCombineCoord /\
    """vec2 pos = _st-0.5;
  vec3 rpos = vec3(0.0, 0.0, -10.0);
  vec3 rdir = normalize(vec3(pos * 3.0, 1.0));
  float d = 0.0;
  for(int i = 0; i < 16; ++i){
    float height = length(_c0);
    d = length(rpos) - (radius+height);
    rpos += d * rdir;
    if (abs(d) < 0.001)break;
  }
  return vec2(atan(rpos.z, rpos.x)+rot, atan(length(rpos.xz), rpos.y));
  """
       /\ (Fn.Fn $ "sphereDisplacement" /\
            [ Fn.Argument "radius" $ H.V $ H.Number 4.0
            , Fn.Argument "rot" $ H.V $ H.Number 0.0
            ]
       )


sphereDisplacement2 :: H.GlslFn
sphereDisplacement2 =
    H.GlslFn
    $ H.FnCombineCoord
    /\
    """
    vec2 pos = _st-0.5;
    vec3 rpos = vec3(0.0, 0.0, -10.0);
    vec3 rdir = normalize(vec3(pos * 3.0, 1.0));
    float d = 0.0;
    for(int i = 0; i < 16; ++i){
        float height = length(_c0);
        d = length(rpos) - (radius+height);
        rpos += d * rdir;
        if (abs(d) < 0.001)break;
    }
    if(d > 0.5) return vec2(0.5,0.5);
    else return vec2(atan(rpos.z, rpos.x)+rot, atan(length(rpos.xz), rpos.y));
    """
       /\ (Fn.Fn $ "sphereDisplacement2" /\
            [ Fn.Argument "radius" $ H.V $ H.Number 4.0
            , Fn.Argument "rot" $ H.V $ H.Number 0.0
            ]
       )


modulateSR :: H.GlslFn
modulateSR =
    H.GlslFn
    $ H.FnCombineCoord
    /\
    """
    vec2 xy = _st - vec2(0.5);
    float angle = rotateOffset + _c0.z * rotateMultiple;
    xy = mat2(cos(angle),-sin(angle), sin(angle),cos(angle))*xy;
    xy*=(1.0/vec2(offset + multiple*_c0.r, offset + multiple*_c0.g));
    xy+=vec2(0.5);
    return xy;
    """
    /\ (Fn.Fn $ "modulateSR" /\
            [ Fn.Argument "multiple" $ H.V $ H.Number 1.0
            , Fn.Argument "offset" $ H.V $ H.Number 1.0
            , Fn.Argument "rotateMultiple" $ H.V $ H.Number 1.0
            , Fn.Argument "rotateOffset" $ H.V $ H.Number 1.0
            ]
       )


knownFns :: Array H.GlslFn
knownFns =
    [ choma
    , sphere
    , sphereDisplacement
    , sphereDisplacement2
    , modulateSR
    ]