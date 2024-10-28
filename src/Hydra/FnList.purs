module Hydra.FnList where

import Prelude


type Entry = { state :: State, group :: String, name :: String, spec :: String, examples :: Array String }


data State
    = TODO
    | DONE


e :: State -> String -> String -> String -> Array String -> Entry
e state group name spec examples = { state, group, name, spec, examples }


fns :: Array Entry
fns =
    [ e TODO "array" "ease" "" []
    , e TODO "array" "fast" "" []
    , e TODO "array" "fit" "" []
    , e TODO "array" "offset" "" []
    , e TODO "array" "smooth" "" []

    , e TODO "audio" "fft" "" []
    , e TODO "audio" "hide" "" []
    , e TODO "audio" "setBins" "" []
    , e TODO "audio" "setCutoff" "" []
    , e TODO "audio" "setScale" "" []
    , e TODO "audio" "setSmooth" "" []

    , e TODO "cai" "gradientShader" "" []
    , e TODO "cai" "productPalette" "" []
    , e TODO "cai" "recolor" "" []
    , e TODO "cai" "smartGradient" "" []
    , e TODO "cai" "watermelonShader" "" []

    , e DONE "blend" "add" "" []
    , e DONE "blend" "blend" "" []
    , e DONE "blend" "diff" "" []
    , e DONE "blend" "layer" "" []
    , e DONE "blend" "mask" "" []
    , e DONE "blend" "mult" "" []
    , e DONE "blend" "sub" "" []

    , e DONE "color" "a" "" []
    , e DONE "color" "b" "" []
    , e DONE "color" "brightness" "" []
    , e DONE "color" "color" "" []
    , e DONE "color" "colorama" "" []
    , e DONE "color" "contrast" "" []
    , e DONE "color" "g" "" []
    , e DONE "color" "hue" "" []
    , e DONE "color" "invert" "" []
    , e DONE "color" "luma" "" []
    , e DONE "color" "posterize" "" []
    , e DONE "color" "r" "" []
    , e DONE "color" "saturate" "" []
    , e DONE "color" "shift" "" []
    , e DONE "color" "sum" "" []
    , e DONE "color" "thresh" "" []

    , e DONE "extsource" "init" "" []
    , e DONE "extsource" "initCam" "" []
    , e DONE "extsource" "initImage" "" []
    , e DONE "extsource" "initScreen" "" []
    , e DONE "extsource" "initStream" "" []
    , e DONE "extsource" "initVideo" "" []

    , e TODO "feed" "array" "" []
    , e TODO "feed" "callGlslFunction" "" []
    , e TODO "feed" "expression" "" []
    , e TODO "feed" "pi" "" []
    , e DONE "feed" "number" "" []

    , e DONE "geometry" "kaleid" "" []
    , e DONE "geometry" "pixelate" "" []
    , e DONE "geometry" "repeat" "" []
    , e DONE "geometry" "repeatX" "" []
    , e DONE "geometry" "repeatY" "" []
    , e DONE "geometry" "rotate" "" []
    , e DONE "geometry" "scale" "" []
    , e DONE "geometry" "scroll" "" []
    , e DONE "geometry" "scrollX" "" []
    , e DONE "geometry" "scrollY" "" []


    , e DONE "modulate" "modulate" "" []
    , e DONE "modulate" "modulateHue" "" []
    , e DONE "modulate" "modulateKaleid" "" []
    , e DONE "modulate" "modulatePixelate" "" []
    , e DONE "modulate" "modulateRepeat" "" []
    , e DONE "modulate" "modulateRepeatX" "" []
    , e DONE "modulate" "modulateRepeatY" "" []
    , e DONE "modulate" "modulateRotate" "" []
    , e DONE "modulate" "modulateScale" "" []
    , e DONE "modulate" "modulateScrollX" "" []
    , e DONE "modulate" "modulateScrollY" "" []

    , e TODO "out" "out" "" []

    , e DONE "source" "gradient"
        "gradient( speed )"
        [ "gradient([1,2,4]).out(o0)" -- gradient sequence at speeds of 1, 2 & 4
        , "gradient(0).r().repeat(16,1).scrollX(0,0.1).out(o0)" -- saw oscillator
        ]
    , e TODO "source" "noise"
        "noise( scale = 10, offset = 0.1 )"
        [ "noise(10, 0.1).out(o0)" -- default
        , """noise( () => Math.sin(time/10)*50 , () => Math.sin(time/2)/500 )
.out(o0)
""" -- noise interpolating between different scales and offsets
        ]
    , e DONE "source" "osc"
        "osc( frequency = 60, sync = 0.1, offset )"
        [ "osc( [1,10,50,100,250,500].fast(2) ).out(o0)" -- frequency
        , "osc( () => Math.sin(time/10) * 100 ).out(o0)" -- frequency 2
        , "osc( 10, [-10,-1,-0.1,0,0.1,1,10], 0 ).out(o0)" -- sync
        , "osc(10,0.1, ({time}) => Math.sin(time/10) * 100 ).out(o0)" -- offset
        ]
    , e DONE "source" "prev" "" []
    , e DONE "source" "shape"
        "shape( sides = 3, radius = 0.3, smoothing = 0.01 )"
        [ "shape(3,0.5,0.001).out(o0)" -- triangle
        , "shape(100,0.5,0.001).out(o0)" -- ellipse
        , "shape(100,0.01,1).invert(()=>Math.sin(time)*2).out(o0)" -- inverting blurry circle
        , """shape(5,0.5,0.1).repeat(19,19)
  .mult(osc(10,1,2))
  .rotate( ({time}) => time%360 )
  .scrollX(1,-0.25)
  .mult(shape(15,0.3,0.01)
  .rotate( ({time}) => time%360 )
  .scrollX(1,-0.25))
  .out(o0)""" --- a... rainbow ball?
        ]
    , e DONE "source" "solid"
        "solid( r, g, b, a = 1 )"
        [ "solid([1,0,0],[0,1,0],[0,0,1],1).out(o0)" -- cycling through red, green and blue
        ]
    , e DONE "source" "src"
        "src( tex )"
        [ "src(o0).modulate(noise(3),0.005).blend(shape(4),0.01).out(o0)" -- feedback
        ]
    , e DONE "source" "voronoi"
        "voronoi( scale = 5, speed = 0.3, blending = 0.3 )"
        [ "voronoi(5,0.3,0.3).out(o0)" -- default
        , "voronoi(25,2,10).color(1,1,0).brightness(0.15).out(o0)" -- fireflies
        ]
    , e TODO "synth" "bpm" "" []
    , e TODO "synth" "height" "" []
    , e TODO "synth" "hush" "" []
    , e TODO "synth" "mouse" "" []
    , e TODO "synth" "render" "" []
    , e TODO "synth" "setFunction" "" []
    , e TODO "synth" "setResolution" "" []
    , e TODO "synth" "speed" "" []
    , e TODO "synth" "time" "" []
    , e TODO "synth" "update" "" []
    , e TODO "synth" "width" "" []

    ]