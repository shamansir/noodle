module Toolkit.Hydra2.Lang.Glsl where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\), type (/\))


import Toolkit.Hydra2.Types as H
import Toolkit.Hydra2.Lang.Fn as Fn


-- examples are from: https://hydra-book.glitch.me/#/glsl


chroma :: H.GlslFn
chroma =
    H.GlslFn
    $ H.FnColor
    /\ H.GlslFnCode
    """float maxrb = max( _c0.r, _c0.b );
   float k = clamp( (_c0.g-maxrb)*5.0, 0.0, 1.0 );
   float dg = _c0.g;
   _c0.g = min( _c0.g, maxrb*0.8 );
   _c0 += vec4(dg - _c0.g);
   return vec4(_c0.rgb, 1.0 - k);
   """
   /\ Fn.fn2 "chroma"
            ( "radius" /\ (H.V $ H.Number 4.0) )
            ( "rot" /\ (H.V $ H.Number 0.0) )


sphere :: H.GlslFn
sphere =
    H.GlslFn
    $ H.FnCoord
    /\ H.GlslFnCode
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
       /\ Fn.fn2 "sphere"
            ( "radius" /\ (H.V $ H.Number 4.0) )
            ( "rot" /\ (H.V $ H.Number 0.0) )


sphereDisplacement :: H.GlslFn
sphereDisplacement =
    H.GlslFn
    $ H.FnCombineCoord
    /\ H.GlslFnCode
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
       /\ Fn.fn2 "sphereDisplacement"
            ( "radius" /\ (H.V $ H.Number 4.0) )
            ( "rot" /\ (H.V $ H.Number 0.0) )


sphereDisplacement2 :: H.GlslFn
sphereDisplacement2 =
    H.GlslFn
    $ H.FnCombineCoord
    /\ H.GlslFnCode
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
       /\ Fn.fn2 "sphereDisplacement2"
            ( "radius" /\ (H.V $ H.Number 4.0) )
            ( "rot" /\ (H.V $ H.Number 0.0) )


modulateSR :: H.GlslFn
modulateSR =
    H.GlslFn
    $ H.FnCombineCoord
    /\ H.GlslFnCode
    """
    vec2 xy = _st - vec2(0.5);
    float angle = rotateOffset + _c0.z * rotateMultiple;
    xy = mat2(cos(angle),-sin(angle), sin(angle),cos(angle))*xy;
    xy*=(1.0/vec2(offset + multiple*_c0.r, offset + multiple*_c0.g));
    xy+=vec2(0.5);
    return xy;
    """
    /\ Fn.fn4 "modulateSR"
            ( "multiple" /\ (H.V $ H.Number 1.0) )
            ( "offset" /\ (H.V $ H.Number 1.0) )
            ( "rotateMultiple" /\ (H.V $ H.Number 1.0) )
            ( "rotateOffset" /\ (H.V $ H.Number 1.0) )


gradientCAI :: H.GlslFn
gradientCAI =
     H.GlslFn
     $ H.FnSrc
     /\ H.GlslFnCode
     """
vec4 buf[8];

// Normalized pixel coordinates (from -1 to 1)
//    vec2 uv = fragCoord/iResolution.xy * 2. - 1.;
//    uv.y *= -1.;

#define buf0sig (1. / (1. + exp(-buf[0])))
#define buf1sig (1. / (1. + exp(-buf[1])))
#define buf2sig (1. / (1. + exp(-buf[2])))
#define buf3sig (1. / (1. + exp(-buf[3])))
#define buf4sig (1. / (1. + exp(-buf[4])))
#define buf5sig (1. / (1. + exp(-buf[5])))
#define buf6sig (1. / (1. + exp(-buf[6])))
#define buf7sig (1. / (1. + exp(-buf[7])))

vec2 coordinate = _st;
float in0 = intensity * 0.1 * sin(0.3  * time * speed);
float in1 = intensity * 0.1 * sin(0.69 * time * speed);
float in2 = intensity * 0.1 * sin(0.44 * time * speed);

//layer 1 *********************************************************************
buf[6] = vec4(coordinate.x, coordinate.y, 0.5 + in0, 0.5 + in1);
buf[7] = vec4(0.5 + in2, sqrt(coordinate.x * coordinate.x + coordinate.y * coordinate.y), 0., 0.);
// layer 2 ********************************************************************
buf[0] = mat4(vec4(0.15798935, -2.8230245, -4.0485225, 5.559854), vec4(-3.8132567, -2.0905786, 0.8464008, -0.35414496), vec4(0.49835995, 1.0014901, -1.8165525, 6.8927507), vec4(1.1554208, -6.040754, 4.0498004, -26.003893))
* buf[6]
+ mat4(vec4(-0.6215526, 1.3650414, 3.866982, -21.346378), vec4(-1.5024208, -2.6894457, 1.3878456, 4.068565), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0))
* buf[7]
+ vec4(-1.1714991, 0.8633618, -2.832041, 5.341836);
buf[1] = mat4(vec4(3.6728961, 3.4556406, 1.2890948, -0.891091), vec4(-1.1152683, -0.18824871, 1.324573, 0.7986717), vec4(1.0294274, -2.1620352, -1.186587, 2.3083494), vec4(-3.5140219, 4.535539, 2.2479877, -5.571291))
* buf[6]
+ mat4(vec4(0.16222078, -5.2829113, -5.244061, 4.105125), vec4(1.3442137, 1.7572293, -0.5369359, -0.12125793), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0))
* buf[7]
+ vec4(0.8421427, -2.9303784, 0.8615611, -1.4667383);

buf[0] = buf0sig;
buf[1] = buf1sig;
// layer 3 ********************************************************************
buf[2] = mat4(vec4(-4.6698623, -1.0911214, 3.5526507, 6.221412), vec4(2.4494207, 1.6892289, -0.104415156, 1.4818819), vec4(2.4043498, 0.7167579, -2.9262938, 1.390429), vec4(-2.6370418, -0.9565374, 6.378253, -1.2293282))
* buf[6]
+ mat4(vec4(-1.8213294, -3.5311048, -7.43784, -0.41914323), vec4(-1.0104917, -3.137278, -0.83638984, 0.28319156), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0))
* buf[7]
+ vec4(2.3352578, 1.2006032, -2.0227838, -2.0087633);
buf[3] = mat4(vec4(-4.458836, -1.2162573, 6.062383, -2.6813662), vec4(-0.5508846, -0.31054375, -0.16497393, 2.2833538), vec4(-3.9579265, 0.3380532, -1.5743831, -0.63811773), vec4(1.0462208, -0.7786392, -1.9073483, -0.3884209))
* buf[6]
+ mat4(vec4(-14.606563, -2.0753143, -4.133394, 3.8478277), vec4(-0.4067336, 1.4517672, 0.799758, -0.49689233), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0))
* buf[7]
+ vec4(5.6508045, -1.5497028, -2.5208669, 0.6735321);
buf[2] = buf2sig;
buf[3] = buf3sig;

// layer 4 *******************************************************************
// implemented as a part of layers 2 and 3
// layer 5 and 6 ********************************************************************
buf[4] = mat4(vec4(-1.5088892, 5.845585, 4.481823, -5.0443144), vec4(7.849486, 4.1395445, -6.5602446, 1.1036118), vec4(6.6682606, -11.55753, 4.763195, 1.455906), vec4(0.017175646, -366.15158, -80.29134, -2.5276334))
* buf[0]
+ mat4(vec4(2.775591, 7.2307186, -9.028725, 5.7900815), vec4(-7.446412, 7.9065814, -9.783361, 1.2135185), vec4(-1.9590125, -22.139872, -15.785561, -3.513498), vec4(-4.946649, -1.2779664, 7.221207, -11.093107))
* buf[1]
+ mat4(vec4(-3.5154085, -12.085035, -7.5107594, -1.8339653), vec4(0.8341089, -8.585301, 13.7843485, -0.3522785), vec4(-4.9421854, 3.1185641, 10.40273, -11.730778), vec4(0.9312287, -10.028536, 5.02363, 0.81166613))
* buf[2]
+ mat4(vec4(0.8054617, 1.3852259, -0.91636896, 0.9319674), vec4(-3.815979, 0.03928483, 2.3691566, 8.696806), vec4(-1.5852077, -62.445934, -11.393516, -8.756885), vec4(-1.2853988, 13.212772, -1.6257201, -5.2855825))
* buf[3]
+ vec4(2.241785, -1.8314902, 0.83580786, 9.41169);
buf[5] = mat4(vec4(0.26124564, 6.4376264, 11.645624, -2.8494694), vec4(-7.9993377, -8.9802, -2.7911298, -3.5069635), vec4(6.4628253, -18.243145, -3.9762654, -0.20418885), vec4(-4.22456, -2.4636898, 3.7532065, 1.8742566))
* buf[0]
+ mat4(vec4(-6.075564, -6.252802, 1.1957424, -1.4090719), vec4(-5.8203125, -4.739796, 8.486904, -0.13786879), vec4(6.7216825, 18.72611, 6.00507, 1.4542285), vec4(-0.95993805, 9.570557, 2.153824, 3.378236))
* buf[1]
+ mat4(vec4(1.6772522, -0.61022216, 7.091971, -1.2360073), vec4(1.236706, -4.970267, -2.2008255, 1.9552149), vec4(3.094974, -4.8698525, 0.40880343, 3.1228032), vec4(8.188464, -3.8818061, -5.5546713, 1.131158))
* buf[2]
+ mat4(vec4(-6.1965737, -7.745663, -7.318301, -1.0420072), vec4(8.857743, 17.735998, -4.3147907, 2.8744252), vec4(-1.0132635, 0.06596738, 4.7702694, 2.134518), vec4(-5.455057, 7.070823, -0.75690097, -3.3348205))
* buf[3]
+ vec4(-1.4140449, -0.085510485, -7.5318046, -1.6154095);
buf[4] = buf4sig;
buf[5] = buf5sig;
// layer 7 and 8 ********************************************************************
buf[6] = mat4(vec4(-15.606012, 5.667645, 15.556916, 5.948859), vec4(-23.811289, -57.638504, 1.2856681, 12.969466), vec4(7.5690074, 0.24498177, -11.368519, -4.0175567), vec4(0.13595544, -12.263578, -36.79266, -0.3176781))
* buf[0]
+ mat4(vec4(-12.067775, -19.733822, 0.0043875263, 5.9834905), vec4(33.6138, 52.55488, 1.0298972, -20.974585), vec4(16.112684, -23.121197, -40.257607, -8.971476), vec4(17.31965, 62.36546, -22.440826, -10.198179))
* buf[1]
+ mat4(vec4(3.4817746, 5.599132, 23.838358, -2.4724288), vec4(-21.209898, 9.769347, 116.00354, 5.175677), vec4(-43.51304, 12.676538, -21.77549, 23.987005), vec4(3.9821281, -14.006245, 11.864449, -1.2515826))
* buf[2]
+ mat4(vec4(5.8689327, 26.391949, 8.38064, -3.0373583), vec4(18.201714, -12.485214, 9.515582, -9.936555), vec4(8.737038, 42.395084, -98.84307, -7.2060304), vec4(-4.8353596, -57.75659, 34.090992, -0.76057786))
* buf[3]
+ mat4(vec4(17.897236, 57.418888, -25.817692, -12.854139), vec4(-333.43958, 12.474519, -22.928097, 17.587284), vec4(-11.044223, -21.18064, -7.4664016, 6.4900517), vec4(-11.78397, -3.8222384, 2.2361667, 5.8452854))
* buf[4]
+ mat4(vec4(5.482705, 30.97732, -14.342825, -4.541368), vec4(11.686139, -9.402538, 82.86699, -6.3102694), vec4(-1.7510675, -17.70653, 10.38284, 2.4797828), vec4(3.1584928, -40.417427, 136.6705, -5.2708864))
* buf[5]
+ vec4(-15.636082, -0.9774143, 27.575691, 13.590046);
buf[7] = mat4(vec4(-12.8582325, -2.9760349, -2.3565636, 1.3976682), vec4(-24.342659, -50.560776, 22.960861, -16.405113), vec4(5.6498, 6.356693, 0.69697034, 4.604504), vec4(0.27611026, 1.6578561, 0.5002827, 12.335777))
* buf[0]
+ mat4(vec4(-16.244108, -2.5964715, -0.71730274, 45.548206), vec4(33.55436, 41.571148, -11.721937, -31.528742), vec4(15.379328, -4.298718, 6.320234, 10.113711), vec4(21.241781, 26.57794, -11.671681, -117.27091))
* buf[1]
+ mat4(vec4(2.8117876, 13.437091, -6.246424, 28.993223), vec4(-29.683706, 15.721599, -9.946332, 23.786804), vec4(-40.6083, -34.22501, 9.716796, 2.6428268), vec4(5.6238184, -8.240437, 5.2400737, -8.701217))
* buf[2]
+ mat4(vec4(6.7769103, 2.3559186, -0.38412517, -37.505245), vec4(18.00992, 34.09309, -17.375113, 8.279464), vec4(14.305624, 4.497131, 5.333335, -42.415443), vec4(-5.0491214, 17.764696, -11.60667, -7.746662))
* buf[3]
+ mat4(vec4(16.946154, 27.063385, -5.7845497, -52.55618), vec4(10.664927, -3.026725, 3.6823318, 12.044233), vec4(-11.567659, -21.509024, 9.306608, 11.384696), vec4(-11.833156, -9.6116495, 2.2288175, -7.5740767))
* buf[4]
+ mat4(vec4(8.126379, 14.71315, -6.240784, -9.104209), vec4(14.132395, 1.3763108, 4.0824256, -17.804401), vec4(1.3440549, -12.342699, 5.3685203, -2.5711007), vec4(-3.2289567, 41.25341, -24.157688, 51.192547))
* buf[5]
+ vec4(-18.787123, -29.089497, 4.4133987, 40.31439);
buf[6] = buf6sig;
buf[7] = buf7sig;

// layer 9 ********************************************************************
buf[0] = mat4(vec4(-0.37223837, 0.21939822, -0.1415498, 0.0), vec4(-2.106104, 0.19532442, 0.22481734, 0.0), vec4(0.4536551, -0.25022385, 0.016776657, 0.0), vec4(0.07051695, -0.11261972, 0.038539752, 0.0))
* buf[0]
+ mat4(vec4(-1.1898454, 0.14878118, 0.19612461, 0.0), vec4(2.766664, -0.7235617, -0.19985063, 0.0), vec4(-1.8502979, -0.82161206, -0.1178217, 0.0), vec4(1.8479801, -0.012557047, -0.27770618, 0.0))
* buf[1]
+ mat4(vec4(0.2225743, -0.1985513, 0.03712178, 0.0), vec4(0.4275036, 0.49749967, -0.06294606, 0.0), vec4(-0.050370935, 1.5264646, 0.30295664, 0.0), vec4(0.42175412, -0.3656762, -0.16419275, 0.0))
* buf[2]
+ mat4(vec4(1.1564953, 0.10743775, -0.07100872, 0.0), vec4(-0.17357533, -0.63636285, -0.25206563, 0.0), vec4(0.8195995, -0.40993538, -0.0032088961, 0.0), vec4(-1.0819085, -0.7051479, -0.06533896, 0.0))
* buf[3]
+ mat4(vec4(3.503318, 0.15870234, -0.29349273, 0.0), vec4(1.3844607, 0.43712336, 0.030473951, 0.0), vec4(-0.6053073, 0.31924763, 0.16967534, 0.0), vec4(-0.37950155, 0.28001186, 0.073448956, 0.0))
* buf[4]
+ mat4(vec4(0.80314213, 0.015278891, -0.16087158, 0.0), vec4(0.88083446, -0.24595416, 0.027351934, 0.0), vec4(-0.4420246, 0.16092822, 0.075462736, 0.0), vec4(-0.27461016, -0.5868031, -0.24838275, 0.0))
* buf[5]
+ mat4(vec4(-1.0109457, -0.89803195, -2.6106067, 0.0), vec4(-0.9218671, -0.39734232, 0.0750759, 0.0), vec4(15.82821, 9.525322, 3.4267645, 0.0), vec4(2.1332054, 1.3592621, -1.978056, 0.0))
* buf[6]
+ mat4(vec4(1.2464583, -1.6644447, -2.5020704, 0.0), vec4(-4.505297, -0.79907095, 0.47479317, 0.0), vec4(-46.21531, -15.580507, -4.5369325, 0.0), vec4(1.4000291, 0.4096961, -0.04745275, 0.0))
* buf[7]
+ vec4(-17.10207, -7.911692, -1.3436741, 0.0);
buf[0] = buf0sig;
return vec4(buf[0].x , buf[0].y , buf[0].z, 1);
     """
    /\ Fn.fn2 "gradientCAI"
            ( "speed" /\ (H.V $ H.Number 1.0) )
            ( "intensity" /\ (H.V $ H.Number 1.0) )


gradient3 :: H.GlslFn
gradient3 =
    H.GlslFn
    $ H.FnSrc
    /\ H.GlslFnCode
    """
	vec2 gradient_start_pos = vec2(0.0, 0.0); // top-left
    vec2 gradient_end_pos = vec2(1.0, 1.0); // bottom-right

    float stops[3];
    vec4 colors[3];
    stops[0] = stop0;
    stops[1] = stop1;
    stops[2] = stop2;
    colors[0] = color0;
    colors[1] = color1;
    colors[2] = color2;

    vec4 fragColor;

    float gradient_startpos_x = gradient_start_pos.x;
    float gradient_endpos_x = gradient_end_pos.x;
    float len = gradient_endpos_x - gradient_startpos_x;
    float x_loc = uv.x;

    	fragColor = mix(colors[0], colors[1], smoothstep(
            gradient_startpos_x + stops[0] * len,
            gradient_startpos_x + stops[1] * len,
            x_loc
        ));
     for (int i = 1; i < 2; i++) {
          fragColor = mix(fragColor, colors[i + 1], smoothstep(
          gradient_startpos_x + stops[i] * len,
          gradient_startpos_x + stops[i + 1] * len,
          x_loc
          ));
     }

     return fragColor;
     """
     /\ Fn.fn6 "gradient3"
            ( "color0" /\ (H.T $ H.Empty) )
            ( "color1" /\ (H.T $ H.Empty) )
            ( "color2" /\ (H.T $ H.Empty) )
            ( "stop0" /\ (H.V $ H.Number 0.0) )
            ( "stop1" /\ (H.V $ H.Number 0.5) )
            ( "stop2" /\ (H.V $ H.Number 1.0) )


gradient4 :: H.GlslFn
gradient4 =
    H.GlslFn
    $ H.FnSrc
    /\ H.GlslFnCode
    """
    vec2 gradient_start_pos = vec2(0.0, 0.0); // top-left
    vec2 gradient_end_pos = vec2(1.0, 1.0); // bottom-right

    float stops[4];
    vec4 colors[4];
    stops[0] = stop0;
    stops[1] = stop1;
    stops[2] = stop2;
    stops[3] = stop3;
    colors[0] = color0;
    colors[1] = color1;
    colors[2] = color2;
    colors[3] = color3;

    vec4 fragColor;

    float gradient_startpos_x = gradient_start_pos.x;
    float gradient_endpos_x = gradient_end_pos.x;
    float len = gradient_endpos_x - gradient_startpos_x;
    float x_loc = uv.x;

    	fragColor = mix(colors[0], colors[1], smoothstep(
          gradient_startpos_x + stops[0] * len,
          gradient_startpos_x + stops[1] * len,
          x_loc
     ));
     for (int i = 1; i < 3; i++) {
          fragColor = mix(fragColor, colors[i + 1], smoothstep(
               gradient_startpos_x + stops[i] * len,
               gradient_startpos_x + stops[i + 1] * len,
               x_loc
          ));
     }

     return fragColor;
     """
     /\ Fn.fn8 "gradient4"
            ( "color0" /\ (H.T $ H.Empty) )
            ( "color1" /\ (H.T $ H.Empty) )
            ( "color2" /\ (H.T $ H.Empty) )
            ( "color3" /\ (H.T $ H.Empty) )
            ( "stop0" /\ (H.V $ H.Number 0.0) )
            ( "stop1" /\ (H.V $ H.Number 0.33) )
            ( "stop2" /\ (H.V $ H.Number 0.66) )
            ( "stop3" /\ (H.V $ H.Number 1.0) )


gradient3CAI :: H.GlslFn
gradient3CAI =
    H.GlslFn
    $ H.FnSrc
    /\ H.GlslFnCode
    """
	  vec2 gradient_start_pos = vec2(0.0, 0.0); // top-left
    vec2 gradient_end_pos = vec2(1.0, 1.0); // bottom-right

	  int modeInt = 7; // could be 3, 5 or 7
    // int modeInt = int(mode);
	  int lastStop = modeInt;

    float stops[7];
    vec4 colors[7];

    vec3 primary_hsv = _rgbToHsv(primary.rgb);
    vec3 secondary_hsv = _rgbToHsv(secondary.rgb);
    vec3 ternary_hsv = _rgbToHsv(ternary.rgb);

    if (modeInt == 3) {
      colors[0] = primary;
      colors[1] = secondary;
      colors[2] = ternary;

    	stops[0] = 0.4;
      stops[1] = 0.7;
      stops[2] = 0.9;
    }

    if (modeInt == 5) {
		  vec3 stop_middle_rgb = _hsvToRgb(vec3((ternary_hsv.x - secondary_hsv.x) / 2.0, ternary_hsv.y, ternary_hsv.z));

    	colors[0] = primary;
        colors[1] = secondary;
        colors[2] = vec4(stop_middle_rgb, secondary.a);
        colors[3] = secondary;
        colors[4] = ternary;

    	  stops[0] = 0.4;
        stops[1] = 0.65;
        stops[2] = 0.7;
        stops[3] = 0.75;
        stops[4] = 0.9;
    }

    if (modeInt == 7) {

		  vec3 stop_first_rgb = _hsvToRgb(vec3(primary_hsv.x + 10.0, min(primary_hsv.y * 2.0, 1.0), primary_hsv.z * 0.3));
      vec3 stop_second_rgb = _hsvToRgb(vec3(primary_hsv.x + 10.0, 1.0, primary_hsv.z * 0.5));
      vec3 stop_middle_rgb = _hsvToRgb(vec3(secondary_hsv.x + abs(secondary_hsv.x  - ternary_hsv.x) / 2.0, ternary_hsv.y, ternary_hsv.z));
      stop_middle_rgb =  mix(secondary.rgb, ternary.rgb, 0.7);
      vec3 stop_last_rgb = _hsvToRgb(vec3(ternary_hsv.x - 10.0, ternary_hsv.y, ternary_hsv.z));

      colors[0] = vec4(stop_first_rgb, primary.a);
      colors[1] = vec4(stop_second_rgb, primary.a);
      colors[2] = secondary;
      colors[3] = vec4(stop_middle_rgb, secondary.a);
      colors[4] = secondary;
      colors[5] = ternary;
      colors[6] = vec4(stop_last_rgb, ternary.a);

    	stops[0] = 0.02;
      stops[1] = 0.4;
      stops[2] = 0.67;
      stops[3] = 0.7;
      stops[4] = 0.73;
      stops[5] = 0.9;
      stops[6] = 0.98;
    }

    vec4 fragColor;

    float gradient_startpos_x = gradient_start_pos.x;
    float gradient_endpos_x = gradient_end_pos.x;
    float len = gradient_endpos_x - gradient_startpos_x;
    //float x_loc = uv.x;
    float x_loc = _st.x;

    vec3 color_ = vec3(1.0, 1.0, 1.0);
    vec3 stopA = _rgbToHsv(color_.rgb);

    fragColor = mix(colors[0], colors[1], smoothstep(
            gradient_startpos_x + stops[0] * len,
            gradient_startpos_x + stops[1] * len,
            x_loc
        ));

    for (int i = 1; i < 7; i++) {
     	if (i < lastStop) {

          fragColor = mix(fragColor, colors[i + 1], smoothstep(
          gradient_startpos_x + stops[i] * len,
          gradient_startpos_x + stops[i + 1] * len,
          x_loc
          ));
        } else { break; }
    }

    return fragColor;
    """
    /\ Fn.fn3 "gradient3CAI"
          ( "primary" /\ (H.T $ H.Empty) )
          ( "secondary" /\ (H.T $ H.Empty) )
          ( "ternary" /\ (H.T $ H.Empty) )


recolorCAI :: H.GlslFn
recolorCAI =
    H.GlslFn
    $ H.FnCombineCoord
    /\ H.GlslFnCode
      """
      vec3 hsv = _rgbToHsv(vec3(_c0));
      float value = hsv.z;
      return fract(vec2(value, _st.y));
      """
    /\ Fn.empty "recolorCAI"


watermelonCAI :: H.GlslFn
watermelonCAI =
    H.GlslFn
    $ H.FnSrc
    /\ H.GlslFnCode
      """
        vec4 buf[8];

        // Normalized pixel coordinates (from -1 to 1)
        //    vec2 uv = fragCoord/iResolution.xy * 2. - 1.;
        //    uv.y *= -1.;

        #define buf0sig (1. / (1. + exp(-buf[0])))
        #define buf1sig (1. / (1. + exp(-buf[1])))
        #define buf2sig (1. / (1. + exp(-buf[2])))
        #define buf3sig (1. / (1. + exp(-buf[3])))
        #define buf4sig (1. / (1. + exp(-buf[4])))
        #define buf5sig (1. / (1. + exp(-buf[5])))
        #define buf6sig (1. / (1. + exp(-buf[6])))
        #define buf7sig (1. / (1. + exp(-buf[7])))

        vec2 coordinate = _st * 2. - 1.;
        float in0 = intensity * 0.1 * sin(0.3  * time * speed);
        float in1 = intensity * 0.1 * sin(0.69 * time * speed);
        float in2 = intensity * 0.1 * sin(0.44 * time * speed);

        //layer 1 *********************************************************************
        buf[6] = vec4(coordinate.x, coordinate.y, 0.5 + in0, 0.5 + in1);
        buf[7] = vec4(0.5 + in2, sqrt(coordinate.x * coordinate.x + coordinate.y * coordinate.y), 0., 0.);
        // layer 2 ********************************************************************
        buf[0] = mat4(vec4(1.2861046, 1.5255139, -7.1725183, -4.801118), vec4(1.463134, 0.6137832, 0.14358509, 2.3431826), vec4(-27.112312, 0.24204023, -6.816938, 5.7869935), vec4(2.5173395, -3.5867562, 2.724099, -0.8675306))
        * buf[6]
        + mat4(vec4(-1.9733338, -6.3440466, 5.876144, -11.172697), vec4(1.2209212, 6.7005653, -7.3256803, 0.12810972), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0))
        * buf[7]
        + vec4(9.071828, -1.6100459, 0.46715444, -0.6035634);
        buf[1] = mat4(vec4(2.195286, 1.049918, 2.408516, -2.13128), vec4(1.183842, 6.290653, -4.8481784, 10.230398), vec4(-7.5289445, -8.719146, -10.263682, 8.731823), vec4(3.111139, -2.8879297, 2.3878467, 3.6501355))
        * buf[6]
        + mat4(vec4(11.863081, 7.8822374, -10.142071, -4.9603367), vec4(-4.896446, 4.5659237, 0.57004553, -8.307976), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0))
        * buf[7]
        + vec4(-0.22611849, -2.945915, 3.8565283, -1.3408977);

        buf[0] = buf0sig;
        buf[1] = buf1sig;
        // layer 3 ********************************************************************
        buf[2] = mat4(vec4(0.26101914, -27.250883, -9.61013, 5.5401683), vec4(-3.578991, -27.335972, -5.1048098, 4.760025), vec4(0.35930714, 22.853672, -7.9215527, 6.036496), vec4(2.9072335, -9.599426, 0.22896555, 2.870207))
        * buf[6]
        + mat4(vec4(0.29027894, -2.9013565, -1.4239044, -21.283358), vec4(-9.624079, -1.794107, 2.0813818, -4.806443), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0))
        * buf[7]
        + vec4(4.2190924, -9.122315, -0.6493183, 7.369281);
        buf[3] = mat4(vec4(11.546875, 9.29744, -7.235964, 0.93743294), vec4(8.016573, -17.698978, 3.3860872, 0.9358041), vec4(-4.7870173, -32.787178, -6.415408, -5.626787), vec4(-0.6533503, -5.4573627, 3.5227933, 5.391688))
        * buf[6]
        + mat4(vec4(-0.78751135, 22.169699, -6.8653693, -18.626322), vec4(5.02163, 3.7542677, -1.9555804, -2.0927753), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0))
        * buf[7]
        + vec4(-0.047049403, 2.2301915, 2.658324, 9.668351);
        buf[2] = buf2sig;
        buf[3] = buf3sig;

        // layer 4 *******************************************************************
        // implemented as a part of layers 2 and 3
        // layer 5 and 6 ********************************************************************
        buf[4] = mat4(vec4(-14.605502, 0.23399517, 6.59498, 10.557976), vec4(2.7027836, 0.002666193, 10.697996, -6.1344743), vec4(-0.36314383, 6.124974, -6.37144, 5.779708), vec4(3.896874, -7.2472916, 4.639256, 1.7759117))
        * buf[0]
        + mat4(vec4(-1.2513268, 0.16784598, -7.721789, -6.2634096), vec4(0.1374537, -4.333525, 2.4827425, -2.8722997), vec4(-0.5228374, 7.8746505, -7.1060076, 3.00033), vec4(-5.9715433, -2.6457279, -1.5898381, 1.4923687))
        * buf[1]
        + mat4(vec4(-3.3729212, -6.2098255, -1.644275, -3.992677), vec4(0.70880413, -4.328083, -1.1788137, -3.9629915), vec4(0.9537155, 3.2684414, -2.167644, 3.8941891), vec4(-3.2902112, 0.07258365, -4.756722, -8.079017))
        * buf[2]
        + mat4(vec4(0.729183, 3.9916856, 2.006164, 5.0556803), vec4(0.90820444, 1.1508486, 1.4730672, 0.47854382), vec4(-1.6351696, -0.5631657, -1.5866659, -15.990851), vec4(5.4235225, 3.9566264, -9.765958, 5.368251))
        * buf[3]
        + vec4(-0.7618782, 0.15857396, 2.7410307, 3.8064167);
        buf[5] = mat4(vec4(7.7613025, -33.188507, 0.3465089, 4.1627088), vec4(-6.9234695, -14.704781, 13.061122, -7.4778304), vec4(-0.87008494, 14.758898, 1.8661101, -5.90413), vec4(-25.179224, 16.279158, 4.549851, 75.24664))
        * buf[0]
        + mat4(vec4(5.0371604, -1.478229, -11.204344, -4.694854), vec4(-9.704287, -4.7387333, 1.7275352, 3.1967845), vec4(17.373302, -5.5033374, -24.44655, -0.4090914), vec4(0.106766194, -3.374636, -3.2729397, 5.7611175))
        * buf[1]
        + mat4(vec4(5.1843266, 0.9345086, -8.359263, -7.6784406), vec4(0.8631667, -0.87610453, 2.5920966, 6.4396973), vec4(9.486691, -9.8212, 9.049328, 3.20068), vec4(-0.2808189, 0.10539463, -5.9625955, 3.5970473))
        * buf[2]
        + mat4(vec4(-0.05716629, -0.2924848, 8.733383, 4.361038), vec4(-1.0319043, 2.198018, 0.05856231, -7.832347), vec4(-1.7596678, -15.045959, -17.233456, 84.09306), vec4(-4.632944, 14.785333, -9.574426, 6.3809743))
        * buf[3]
        + vec4(-8.628934, 3.8215384, 4.8875446, 3.5515535);
        buf[4] = buf4sig;
        buf[5] = buf5sig;
        // layer 7 and 8 ********************************************************************
        buf[6] = mat4(vec4(-7.766892, -16.013279, 7.002536, 40.88031), vec4(-3.142986, 0.45606878, 4.0641675, -9.120614), vec4(-1.0108374, 2.1113925, -6.1132984, 0.36139733), vec4(8.233213, -24.95016, 7.3445015, 17.67893))
        * buf[0]
        + mat4(vec4(-1.3596867, -8.362146, -3.173567, 2.0331123), vec4(6.8059745, 1.3065897, 0.17285182, -0.386861), vec4(2.8571618, -12.923676, 17.411291, -4.5148473), vec4(-0.97297513, 1.4158757, 0.21970429, 4.1699433))
        * buf[1]
        + mat4(vec4(2.553137, -2.925799, -12.160719, -9.659747), vec4(-0.79371166, 4.857264, -5.3970084, 2.4020197), vec4(1.1553055, -9.001468, 14.52013, -25.68672), vec4(-3.6449926, -6.130829, -10.324427, -12.907425))
        * buf[2]
        + mat4(vec4(-1.6870475, 2.4665504, 1.7946846, 0.661618), vec4(-1.0322174, 7.603317, -4.254132, 7.6042547), vec4(-5.3262973, 7.404934, -30.310528, -1.0671401), vec4(4.5463285, 17.134487, 10.0499525, -4.471233))
        * buf[3]
        + mat4(vec4(6.8503857, 4.3012123, 16.991276, -27.686306), vec4(1.4814026, -2.5663457, -17.029852, -3.8628333), vec4(1.5890043, -12.005112, 6.8010745, 16.93117), vec4(-1.6270804, -2.7097423, -0.017278401, 4.3496776))
        * buf[4]
        + mat4(vec4(-2.6901076, -0.10961132, -2.7362561, -18.192595), vec4(1.8795626, 4.381529, -22.859179, -8.187753), vec4(-0.47566342, -1.9801006, 14.484972, 6.466274), vec4(1.9014528, 5.429815, 10.88709, -0.12985411))
        * buf[5]
        + vec4(-2.2617304, 2.4453385, -3.9633543, 8.388752);
        buf[7] = mat4(vec4(7.009063, 85.45689, -10.749413, 4.876872), vec4(-20.058754, -11.359139, 0.025702428, -10.269903), vec4(1.5903428, 1.1007673, -9.778767, 1.4234898), vec4(48.29426, -6.534638, -51.74366, 11.628172))
        * buf[0]
        + mat4(vec4(0.9815393, -1.0851281, 4.7514486, -5.0391183), vec4(4.757276, 6.0278406, -1.6730795, 6.359422), vec4(-9.798415, 6.078175, -3.443065, 5.9555635), vec4(0.14293458, 2.1635673, 5.4873066, -2.9637206))
        * buf[1]
        + mat4(vec4(0.037170745, -5.7946925, 0.972805, -3.7335134), vec4(-3.4841716, -3.8743663, 0.9025023, -1.9254402), vec4(-1.5218388, 2.132073, -23.156841, -4.180814), vec4(-17.218805, -11.302172, 4.8323092, 7.806347))
        * buf[2]
        + mat4(vec4(-15.9328785, 5.9368353, -2.6668377, -11.2426405), vec4(-3.7602496, -2.3193603, -1.9841781, -6.1601996), vec4(35.20623, -0.5423199, 15.182161, -3.7123358), vec4(19.393852, 23.378147, 1.7813851, 13.133501))
        * buf[3]
        + mat4(vec4(15.370007, 2.6576698, 4.404216, 10.406783), vec4(3.3044858, 3.4533582, 2.7226963, -8.317185), vec4(-41.688927, 13.266089, -0.015771875, 42.35942), vec4(2.136933, -6.06612, 2.3396618, -1.4731926))
        * buf[4]
        + mat4(vec4(-4.6056848, 10.670126, 3.847859, 8.598114), vec4(-4.2699065, 5.5472865, -1.064826, -10.987815), vec4(-6.414777, -3.6670558, 0.8988497, 1.3321666), vec4(10.820698, 1.3840607, -5.396361, 14.81882))
        * buf[5]
        + vec4(10.518701, -7.876456, -6.39715, -11.081563);
        buf[6] = buf6sig;
        buf[7] = buf7sig;

        // layer 9 ********************************************************************
        buf[0] = mat4(vec4(6.1392565, 1.3980272, -8.122245, 0.0), vec4(2.9661536, 1.2601666, 2.5735781, 0.0), vec4(3.3165007, 0.7656108, 0.3386268, 0.0), vec4(-5.111672, -1.9740047, -4.692353, 0.0))
        * buf[0]
        + mat4(vec4(-4.565195, -4.086584, -2.912243, 0.0), vec4(-2.477629, 0.5393397, -0.8734828, 0.0), vec4(-4.1505938, 1.2248064, 2.5643477, 0.0), vec4(-0.49676016, 0.585036, -0.29764664, 0.0))
        * buf[1]
        + mat4(vec4(-0.6087556, 0.39478865, -1.2404823, 0.0), vec4(-0.37065446, -0.41645864, -0.7715677, 0.0), vec4(0.4654949, 0.59216607, 0.7547539, 0.0), vec4(0.99702036, -0.9418359, -1.0192825, 0.0))
        * buf[2]
        + mat4(vec4(0.033533752, -0.066628054, -0.4295512, 0.0), vec4(2.2258203, 2.1665874, 1.4390037, 0.0), vec4(0.50036025, -3.0169911, 2.3293712, 0.0), vec4(-6.742268, -3.144173, 1.1610886, 0.0))
        * buf[3]
        + mat4(vec4(-4.821113, -3.2717588, -1.7157474, 0.0), vec4(-2.5973628, 0.2239039, -1.0064033, 0.0), vec4(-2.8320718, -2.054768, -1.3171085, 0.0), vec4(-1.1379724, -3.827478, -0.6299828, 0.0))
        * buf[4]
        + mat4(vec4(-0.80007505, -0.5715372, -1.606471, 0.0), vec4(-0.3933879, 0.8925263, 1.098601, 0.0), vec4(1.0905372, 0.8873729, 0.94111615, 0.0), vec4(-0.82396895, 1.2094749, 1.2671765, 0.0))
        * buf[5]
        + mat4(vec4(1.7017696, -1.330418, -5.2155447, 0.0), vec4(-1.832737, -2.4870281, -2.192155, 0.0), vec4(3.562882, 4.188941, 3.707975, 0.0), vec4(-2.687533, -1.6273785, -1.2798731, 0.0))
        * buf[6]
        + mat4(vec4(1.5489951, 2.404655, 1.4699396, 0.0), vec4(4.608174, 2.675385, 2.017389, 0.0), vec4(3.3088732, 8.559673, 1.5672995, 0.0), vec4(2.158764, 1.803782, 1.7574296, 0.0))
        * buf[7]
        + vec4(4.886131, 1.969801, 3.711297, 0.0);
        buf[0] = buf0sig;
        return vec4(buf[0].x , buf[0].y , buf[0].z, 1);

        // Normalized pixel coordinates (from -1 to 1)
        //vec2 uv = fragCoord/iResolution.xy * 2. - 1.;
        //uv.y *= -1.;

        // Output to screen
        //fragColor = cppn_fn(uv, 0.1 * sin(0.3 * iTime), 0.1 * sin(0.69*iTime), 0.1 * sin(0.44 * iTime));
      """
    /\ Fn.fn2 "watermelonCAI"
            ( "speed" /\ (H.V $ H.Number 1.0) )
            ( "intensity" /\ (H.V $ H.Number 1.0) )


knownFns :: Array H.GlslFn
knownFns =
    [ chroma
    , sphere
    , sphereDisplacement
    , sphereDisplacement2
    , modulateSR
    , gradientCAI
    , gradient3
    , gradient4
    , gradient3CAI
    , recolorCAI
    , watermelonCAI
    ]


knownFnsMap ∷ Map String H.GlslFn
knownFnsMap =
  Map.fromFoldable
    $ (\glslFn@(H.GlslFn (_ /\ _ /\ fn)) -> Fn.name fn /\ glslFn) <$> knownFns