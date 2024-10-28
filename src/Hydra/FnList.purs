module Hydra.FnList where

import Prelude


type Entry =
    { ord :: Int
    , state :: State
    , group :: String
    , name :: String
    , spec :: String
    , examples :: Array String
    }


data State
    = TODO
    | DONE


e :: State -> String -> String -> Int -> String -> Array String -> Entry
e state group name ord spec examples = { state, group, name, ord, spec, examples }


fns :: Array Entry
fns =
    {- Array -}

    [ e TODO "array" "ease" 802
        "ease( ease = 'linear' )"
        [ "shape(4).rotate([-3.14,3.14].ease('easeInOutCubic')).out(o0)"
        ]
    , e TODO "array" "fast" 800
        "fast( speed = 1 )"
        [ "osc([10,30,60].fast(2),0.1,1.5).out(o0)"
        , """// argument less than 1 makes transition slower
osc([10,30,60].fast(0.5),0.1,1.5).out(o0)"""
        ]
    , e TODO "array" "fit" 804
        "fit( low = 0, high = 1 )"
        [ "shape().scrollX([0,1,2,3,4].fit(-0.2,0.2)).out(o0)"
        ]
    , e TODO "array" "offset" 803
        "offset( offset = 0.5 )"
        [ """shape(999).scrollY(.2).scrollX([-0.2,0.2])
  .add(
  shape(4).scrollY(-.2).scrollX([-0.2,0.2].offset(0.5))
  ).out(o0)"""
        ]
        , e TODO "array" "smooth" 801
        "smooth( smooth = 1 )"
        [ "shape(999).scrollX([-0.2,0.2].smooth()).out(o0)"
        ]

    {- Audio -}

    , e TODO "audio" "fft" 900
        "fft = Array(4)"
        [ "osc().modulate(noise(3),()=>a.fft[0]).out(o0)"
        ]
    , e TODO "audio" "_hide" 905 "" [ "" ]
    , e TODO "audio" "_show" 906 "" [ "" ]
    , e TODO "audio" "setBins" 903
        "setBins( numBins = 4 )"
        [ """// change color with hissing noise
a.setBins(8)
osc(60,0.1,()=>a.fft[7]*3).modulate(noise(3),()=>a.fft[0]).out(o0)"""
        ]
    , e TODO "audio" "setCutoff" 902
        "setCutoff( cutoff = 2 )"
        [ """// threshold
a.setCutoff(4)
osc().modulate(noise(3),()=>a.fft[0]).out(o0)"""
        ]
    , e TODO "audio" "setScale" 904
        "setScale( scale = 10 )"
        [ """// the smaller the scale is, the bigger the output is
a.setScale(5)
osc().modulate(noise(3),()=>a.fft[0]).out(o0)"""
        ]
    , e TODO "audio" "setSmooth" 901
        "setSmooth( smooth = 0.4 )"
        [ "a.setSmooth(0.8)"
        ]

    {- CAI -}

    , e TODO "cai" "gradientShader" 1002 "" []
    , e TODO "cai" "productPalette" 1000 "" []
    , e TODO "cai" "recolor" 1001 "" []
    , e TODO "cai" "smartGradient" 1004 "" []
    , e TODO "cai" "watermelonShader" 1003 "" []

    {- Blend -}

    , e DONE "blend" "add" 400
        "add( texture, amount = 1 )"
        [ "shape().scale(0.5).add(shape(4),[0,0.25,0.5,0.75,1]).out(o0)"
        , "osc(9,0.1,1).add(osc(13,0.5,5)).out(o0)"
        ]
    , e DONE "blend" "blend" 403
        "blend( texture, amount = 0.5 )"
        [ "shape().scale(0.5).blend(shape(4),[0,0.25,0.5,0.75,1]).out(o0)"
        , "osc(9,0.1,1).blend(osc(13,0.5,5)).out()"
        , """// motion-blur like feedback
osc().thresh().blend(o0,0.9).out(o0)"""
        ]
    , e DONE "blend" "diff" 405
        "diff( texture )"
        [ "osc(9,0.1,1).diff(osc(13,0.5,5)).out(o0)"
        , """osc(1,1,2)
  .diff(shape(6,1.1,0.01)
        .scale(()=>Math.sin(time)*0.05 + 0.9)
        .kaleid(15)
        .rotate(()=>time%360))
  .out()"""
        ]
    , e DONE "blend" "layer" 402
        "layer( texture )"
        [ "solid(1,0,0,1).layer(shape(4).color(0,1,0,()=>Math.sin(time*2))).out(o0)"
        , "osc(30).layer(osc(15).rotate(1).luma()).out(o0)"
        ]
    , e DONE "blend" "mask" 406
        "mask( texture )"
        [ "gradient(5).mask(voronoi(),3,0.5).invert([0,1]).out(o0)"
        , """// mask is transparent; compare with mult
osc()
  .layer(osc(30,0.1,2).mask(shape(4)))
  .out(o0)", "// algae pulse
osc(10,-0.25,1).color(0,0,1).saturate(2).kaleid(50)
  .mask(noise(25,2).modulateScale(noise(0.25,0.05)))
  .modulateScale(osc(6,-0.5,2).kaleid(50))
  .mult(osc(3,-0.25,2).kaleid(50))
  .scale(0.5,0.5,0.75)
  .out(o0)"""
        ]
    , e DONE "blend" "mult" 404
        "mult( texture, amount = 1 )"
        [ "osc(9,0.1,2).mult(osc(13,0.5,5)).out()"
        , """// mult is *not* transparent; compare with mask
osc()
  .layer(osc(30,0.1,2).mult(shape(4)))
  .out(o0)"""
        ]
    , e DONE "blend" "sub" 401
        "sub( texture, amount = 1 )"
        [ "osc().sub(osc(6)).out(o0)"
        , """// color remapping
osc(6,0,1.5).modulate(noise(3).sub(gradient()),1).out(o0)"""
        ]

    {- Color -}

    , e DONE "color" "a" 316 "" []
    , e DONE "color" "b" 315
            "b( scale = 1, offset )"
            [ "osc(60,0.1,1.5).layer(gradient().colorama(1).b()).out(o0)"
            ]
    , e DONE "color" "brightness" 305
        "brightness( amount = 0.4 )"
        [ """osc(20,0,2)
  .brightness( () => Math.sin(time) )
  .out(o0)"""
        , """// scaling noise value to 0-1
noise().brightness(1).color(0.5,0.5,0.5).out(o0)"""
        ]
    , e DONE "color" "color" 308
        "color( r = 1, g = 1, b = 1, a = 1 )"
        [ "osc().color(1,0,3).out(o0)"
        ]
    , e DONE "color" "colorama" 311
        "colorama( amount = 0.005 )"
        [ """// // 20Hz oscillator source
// // color sequence of Red, Green, Blue, White, Black
// // colorama sequence of 0.005, 0.5, 1.0 at 1/8 speed
// // output to buffer o0
osc(20)
  .color([1,0,0,1,0],[0,1,0,1,0],[0,0,1,1,0])
  .colorama([0.005,0.33,0.66,1.0].fast(0.125))
  .out(o0)"""
        , """// negative value is less harsh
osc(30,0.1,1).colorama(-0.1).out(o0)"""
        ]
    , e DONE "color" "contrast" 304
        "contrast( amount = 1.6 )"
        [ """// 20Hz oscillator with contrast interpolating between 0.0-5.0
osc(20).contrast( () => Math.sin(time) * 5 ).out(o0)"""
        ]
    , e DONE "color" "g" 314
        "g( scale = 1, offset )"
        [ "osc(60,0.1,1.5).layer(gradient().g()).out(o0)"
        ]
    , e DONE "color" "hue" 310
        "hue( hue = 0.4 )"
        [ "osc(30,0.1,1).hue(() => Math.sin(time)).out(o0)"
        ]
    , e DONE "color" "invert" 303
        "invert( amount = 1 )"
        [ "solid(1,1,1).invert([0,1]).out(o0)"
        , """osc(4,0.1,2).invert().luma().invert()
  .layer(osc(4,0.1,2).luma()
         .mask(shape(2,0.5).scrollY(-0.25))).out(o0)"""
        ]
    , e DONE "color" "luma" 306
        "luma( threshold = 0.5, tolerance = 0.1 )"
        [ "osc(10,0,1).luma(0.5,0.1).out(o0)"
        , "osc(10,0,[0,0.5,1,2]).luma([0.1,0.25,0.75,1].fast(0.25),0.1).out(o0)"
        , """// luma is transparent; compare with thresh
osc(30).layer(osc(15).rotate(1).luma()).out(o0)"""
        ]
    , e DONE "color" "posterize" 301
        "posterize( bins = 3, gamma = 0.6 )"
        [ """// static gradient posterized, varying bins
gradient(0).posterize( [1, 5, 15, 30] , 0.5 ).out(o0)"""
        , """// static gradient posterized, varying gamma
gradient(0).posterize( 3, [0.1, 0.5, 1.0, 2.0] ).out(o0)"""
        , """// posterize (top); compare with pixelate (bottom)
osc().posterize(3,1)
  .layer(osc().pixelate(16,1)
    .mask(shape(2,0.5,0.001).scrollY(-0.25)))
  .out(o0)"""
        ]
    , e DONE "color" "r" 313
        "r( scale = 1, offset )"
        [ "osc(60,0.1,1.5).layer(gradient().r()).out(o0)"
        ]
    , e DONE "color" "saturate" 309
        "saturate( amount = 2 )"
        [ "osc(10,0,1).saturate( () => Math.sin(time) * 10 ).out(o0)"
        ]
    , e DONE "color" "shift" 302
        "shift( r = 0.5, g, b, a )"
        [ "osc().shift(0.1,0.9,0.3).out()"
        ]
    , e DONE "color" "sum" 312 "" []
    , e DONE "color" "thresh" 307
        "thresh( threshold = 0.5, tolerance = 0.04 )"
        [ "noise(3,0.1).thresh(0.5,0.04).out(o0)"
        , """noise(3,0.1)
  .thresh( ()=>Math.sin(time/2) , [0.04,0.25,0.75,1].fast(0.25) )
  .out(o0)"""
        , """// thresh is *not* transparent; compare with luma
osc(30).layer(osc(15).rotate(1).thresh()).out(o0)"""
        ]

    {- Ext. Source -}

    , e DONE "extsource" "init" 603
        "init( options )"
        [ """// load canvas
canvas = document.createElement("canvas")
canvas.width = 200
canvas.height = 200
ctx = canvas.getContext("2d")
ctx.fillStyle = "crimson"
ctx.fillRect(100,50,100,100)
s0.init({src:canvas})
src(s0).modulate(osc().kaleid(999)).out(o0)"""
        ]
    , e DONE "extsource" "initCam" 600
        "initCam( index )"
        [ """s0.initCam()
src(s0).invert().out(o0)"""
        ]
    , e DONE "extsource" "initImage" 601
        "initImage( url )"
        [ """s0.initImage("https://upload.wikimedia.org/wikipedia/commons/2/25/Hydra-Foto.jpg")
osc(6).modulate(src(s0),1).out(o0)"""
        ]
    , e DONE "extsource" "initScreen" 605
        ""
        [ """// select a window
s0.initScreen()
src(s0).colorama(0.5).out(o0)"""
        ]
    , e DONE "extsource" "initStream" 604 "" []
    , e DONE "extsource" "initVideo" 602
        "initVideo( url )"
        [ """// default
s0.initVideo("https://media.giphy.com/media/AS9LIFttYzkc0/giphy.mp4")
src(s0).modulate(noise(3)).out(o0)"""
        ]

    , e TODO "feed" "array" 2 "" []
    , e TODO "feed" "callGlslFunction" 4 "" []
    , e TODO "feed" "expression" 3 "" []
    , e TODO "feed" "pi" 1 "" []
    , e DONE "feed" "number" 0 "" []

    {- Geometry -}

    , e DONE "geometry" "kaleid" 206
        "kaleid( nSides = 4 )"
        [ "osc(25,-0.1,0.5).kaleid(50).out(o0)"
        , "osc(25,-0.1,0.5).kaleid(4).kaleid(4).out(o0)"
        ]
    , e DONE "geometry" "pixelate" 202
        "pixelate( pixelX = 20, pixelY = 20 )"
        [ "noise().pixelate(20,20).out(o0)"
        , "noise().pixelate(2000,1).out(o0)"
        , """noise()
  .mult(osc(10,0.25,1))
  .scrollY(1,0.25)
  .pixelate([100,40,20,70].fast(0.25))
  .modulateRotate(src(o0).scale(0.5),0.125)
  .diff(src(o0).rotate([-0.05,0.05].fast(0.125)))
    .out(o0)"""
        ]
    , e DONE "geometry" "repeat" 203
        "repeat( repeatX = 3, repeatY = 3, offsetX, offsetY )"
        [ "shape().repeat(3.0, 3.0, 0.0, 0.0).out()"
        , """// dogtooth factory
shape(1.25,0.5,0.25)
  .repeat(3, 3)
  .scale(2)
  .repeat(5, 5, () => Math.sin(time), () => Math.sin(time/2))
  .out(o0)"""
        ]
    , e DONE "geometry" "repeatX" 204
        "repeatX( reps = 3, offset )"
        [ "shape().repeatX(3.0, 0.0).out()"
        , """osc(5,0,1)
  .rotate(1.57)
  .repeatX([1,2,5,10], ({time}) => Math.sin(time))
  .out()"""
        ]
    , e DONE "geometry" "repeatY" 205
        "repeatY( reps = 3, offset )"
        [ "shape().repeatY(3.0, 0.0).out()"
        , """osc(5,0,1)
  .repeatY([1,2,5,10], ({time}) => Math.sin(time))
  .out()"""
        ]
    , e DONE "geometry" "rotate" 200
        "rotate( angle = 10, speed )"
        [ """// constant rotation
osc(50).rotate( () => time%360 ).out(o0)", "// modulate rotation speed
osc(10,1,1)
  .rotate( () => time%360, () => Math.sin(time*0.1)*0.05 )
  .out(o0)""" ]
    , e DONE "geometry" "scale" 201
        "scale( amount = 1.5, xMult = 1, yMult = 1, offsetX = 0.5, offsetY = 0.5 )"
        [ "shape().scale(1.5,1,1).out(o0)"
        , """// flower
shape().scale(1.5,[0.25,0.5,0.75,1].fast(0.25),[3,2,1])
  .invert([0,1].fast(0.25))
  .kaleid(5)
  .kaleid(12)
  .scale( ()=>Math.sin(time/5)*0.5 )
  .rotate(1,1)
  .out(o0)"""
        ]
    , e DONE "geometry" "scroll" 207
        "scroll( scrollX = 0.5, scrollY = 0.5, speedX, speedY )"
        [ "shape(3).scroll(0.1,-0.3).out(o0)" ]
    , e DONE "geometry" "scrollX" 208
        "scrollX( scrollX = 0.5, speed )"
        [ "osc(10,0,1).scrollX(0.5,0).out(o0)"
        , """// x position
osc(10,0,1).scrollX([0,0.25,0.5,0.75,1].fast(4),0).out(o0)"""
        , """// scroll speed
gradient(1).scrollX(0, () => Math.sin(time*0.05)*0.05 ).out(o0)", "gradient(0.125)
  .scrollX(0, ({time}) => Math.sin(time*0.05)*0.05 )
  .scrollY(0, ({time}) => Math.sin(time*0.01)*-0.07 )
  .pixelate([5,2,10],[15,8])
  .scale(0.15)
  .modulate(noise(1,0.25))
  .out()"""
        ]
    , e DONE "geometry" "scrollY" 209
        "scrollY( scrollY = 0.5, speed )"
        [ "osc(10,0,1).scrollY(0.5,0).out(o0)"
        , """// y position
osc(10,0,1).scrollY([0,0.25,0.5,0.75,1].fast(4),0).out(o0)"""
        , """// scroll speed
gradient(1).scrollY(0, ({time}) => Math.sin(time*0.05)*0.05 ).out()", "gradient(0.125)
  .scrollX(0, () => Math.sin(time*0.05)*0.05 )
  .scrollY(0, () => Math.sin(time*0.01)*-0.07 )
  .pixelate([5,2,10],[15,8])
  .scale(0.15)
  .modulate(noise(1,0.25))
  .out()"""
        ]

    {- Modulate -}

    , e DONE "modulate" "modulate" 506
        "modulate( texture, amount = 0.1 )"
        [ """// chocolate whirlpool
voronoi()
  .color(0.9,0.25,0.15)
  .rotate(({time})=>(time%360)/2)
  .modulate(osc(25,0.1,0.5)
              .kaleid(50)
              .scale(({time})=>Math.sin(time*1)*0.5+1)
              .modulate(noise(0.6,0.5)),
              0.5)
  .out(o0)"""
        , """// color remapping
osc(3,0,2)
  .modulate(noise().add(gradient(),-1),1)
  .out(o0)"""
        ]
    , e DONE "modulate" "modulateHue" 510
        "modulateHue( texture, amount = 1 )"
        [ """src(o0)
  .modulateHue(src(o0).scale(1.01),1)
  .layer(osc(4,0.5,2).mask(shape(4,0.5,0.001)))
  .out(o0)"""
        ]
    , e DONE "modulate" "modulateKaleid" 503
        "modulateKaleid( texture, nSides = 4 )"
        [ """osc(9,-0.1,0.1)
  .modulateKaleid(osc(11,0.5,0),50)
  .scale(0.1,0.3)
  .modulate(noise(5,0.1))
  .mult(solid(1,1,0.3))
  .out(o0)", "  osc(10,0.1,2)
  .modulateKaleid(osc(16).kaleid(999),1)
  .out(o0)"""
        ]
    , e DONE "modulate" "modulatePixelate" 508
        "modulatePixelate( texture, multiple = 10, offset = 3 )"
        [ """// what lies beneath
voronoi(10,1,5).brightness(()=>Math.random()*0.15)
  .modulatePixelate(noise(25,0.5),100)
  .out(o0)", "noise(3).modulatePixelate(noise(3).pixelate(8,8),1024,8)
  .out(o0)"""
        ]
    , e DONE "modulate" "modulateRepeat" 500
        "modulateRepeat( texture, repeatX = 3, repeatY = 3, offsetX = 0.5, offsetY = 0.5 )"
        [ """shape(4,0.9)
  .mult(osc(3,0.5,1))
  .modulateRepeat(osc(10), 3.0, 3.0, 0.5, 0.5)
  .out(o0)"""
        ]
    , e DONE "modulate" "modulateRepeatX" 501
        "modulateRepeatX( texture, reps = 3, offset = 0.5 )"
        [ """// straight lines illusion
shape(4,0.9)
  .mult(osc(4,0.25,1))
  .modulateRepeatX(osc(10), 5.0, ({time}) => Math.sin(time) * 5)
  .scale(1,0.5,0.05)
  .out(o0)"""
        ]
    , e DONE "modulate" "modulateRepeatY" 502
        "modulateRepeatY( texture, reps = 3, offset = 0.5 )"
        [ """// morphing grid
shape(4,0.9)
  .mult(osc(4,0.25,1))
  .modulateRepeatY(osc(10), 5.0, ({time}) => Math.sin(time) * 5)
  .scale(1,0.5,0.05)
  .out(o0)"""
        ]
    , e DONE "modulate" "modulateRotate" 509
        "modulateRotate( texture, multiple = 1, offset )"
        [ """// wormhole
voronoi(100,3,5)
  .modulateRotate(osc(1,0.5,0).kaleid(50).scale(0.5),15,0)
  .mult(osc(50,-0.1,8).kaleid(9))
  .out(o0)"""
        , "osc().modulateRotate(shape(999,0.3,0.5),1.57).out(o0)"
        ]
    , e DONE "modulate" "modulateScale" 507
        "modulateScale( texture, multiple = 1, offset = 1 )"
        [ """// cosmic radiation
gradient(5).repeat(50,50).kaleid([3,5,7,9].fast(0.5))
  .modulateScale(osc(4,-0.5,0).kaleid(50).scale(0.5),15,0)
  .out(o0)"""
        , """// perspective
shape(4).modulateScale(gradient().g(),2,0.5).out(o0)"""
        ]
    , e DONE "modulate" "modulateScrollX" 504
        "modulateScrollX( texture, scrollX = 0.5, speed )"
        [ """voronoi(25,0,0)
  .modulateScrollX(osc(10),0.5,0)
  .out(o0)", "// different scroll and speed
voronoi(25,0,0)
  .modulateScrollX(osc(10),0.5,0.25)
  .out(o0)"""
        ]
    , e DONE "modulate" "modulateScrollY" 505
        "modulateScrollY( texture, scrollY = 0.5, speed )"
        [ """voronoi(25,0,0)
  .modulateScrollY(osc(10),0.5,0)
  .out(o0)", "// different scroll and speed
voronoi(25,0,0)
  .modulateScrollY(osc(10),0.5,0.25)
  .out(o0)"""
        ]

    {- Out -}

    , e TODO "out" "out" 1200 "" []

    {- Source -}

    , e DONE "source" "gradient" 104
        "gradient( speed )"
        [ """// gradient sequence at speeds of 1, 2 & 4
gradient([1,2,4]).out(o0)"""
        , """// saw oscillator
gradient(0).r().repeat(16,1).scrollX(0,0.1).out(o0)"""
        ]
    , e TODO "source" "noise" 100
        "noise( scale = 10, offset = 0.1 )"
        [ "noise(10, 0.1).out(o0)"
        , """\\ noise interpolating between different scales and offsets
noise( () => Math.sin(time/10)*50 , () => Math.sin(time/2)/500 )
.out(o0)
"""
        ]
    , e DONE "source" "osc" 102
        "osc( frequency = 60, sync = 0.1, offset )"
        [ """// frequency
osc( [1,10,50,100,250,500].fast(2) ).out(o0)"""
        , """// frequency 2
osc( () => Math.sin(time/10) * 100 ).out(o0)"""
        , """// sync
osc( 10, [-10,-1,-0.1,0,0.1,1,10], 0 ).out(o0)"""
        , """// offset
osc(10,0.1, ({time}) => Math.sin(time/10) * 100 ).out(o0)"""
        ]
    , e DONE "source" "prev" 107 "" []
    , e DONE "source" "shape" 103
        "shape( sides = 3, radius = 0.3, smoothing = 0.01 )"
        [ """// triangle
shape(3,0.5,0.001).out(o0)"""
        , """// ellipse
shape(100,0.5,0.001).out(o0)"""
        , """// inverting blurry circle
shape(100,0.01,1).invert(()=>Math.sin(time)*2).out(o0)"""
        , """\\ a... rainbow ball?
shape(5,0.5,0.1).repeat(19,19)
  .mult(osc(10,1,2))
  .rotate( ({time}) => time%360 )
  .scrollX(1,-0.25)
  .mult(shape(15,0.3,0.01)
  .rotate( ({time}) => time%360 )
  .scrollX(1,-0.25))
  .out(o0)"""
        ]
    , e DONE "source" "solid" 106
        "solid( r, g, b, a = 1 )"
        [ """// cycling through red, green and blue
solid([1,0,0],[0,1,0],[0,0,1],1).out(o0)"""
        ]
    , e DONE "source" "src" 105
        "src( tex )"
        [ """// feedback
src(o0).modulate(noise(3),0.005).blend(shape(4),0.01).out(o0)"""
        ]
    , e DONE "source" "voronoi" 101
        "voronoi( scale = 5, speed = 0.3, blending = 0.3 )"
        [ "voronoi(5,0.3,0.3).out(o0)"
        , """// fireflies
voronoi(25,2,10).color(1,1,0).brightness(0.15).out(o0)"""
        ]

    {- Synth -}

    , e TODO "synthsettings" "bpm" 705
        "bpm = 30"
        [ """// change array speed
bpm = 60
osc(60,0.1,[0,1.5]).out(o0)"""
        , """// change array speed
bpm = 15
osc(60,0.1,[0,1.5]).out(o0)"""
        ]
    , e TODO "synthsettings" "height" 708
        "height"
        [ "shape(99).scrollY(() => -mouse.y / height).out(o0)"
        ]
    , e TODO "synthsettings" "hush" 703
        "hush(  )"
        [ """// clear the buffers
osc().out(o0)
hush()"""
        ]
    , e TODO "synthsettings" "mouse" 710
        "mouse = { x, y }"
        [ """shape(99).scroll(
    () => -mouse.x / width,
    () => -mouse.y / height)
    .out(o0)"""
        ]
    , e TODO "synthsettings" "render" 700
        "render( texture = all )"
        [ """osc(30,0.1,1.5).out(o0)
noise().out(o1)
solid(1).out(o2)
gradient().out(o3)
render()", "// specify display buffer
voronoi().out(o1)
render(o1)"""
        ]
    , e TODO "synthsettings" "setFunction" 704
        "setFunction( options )"
        [ """// from https://www.shadertoy.com/view/XsfGzn
setFunction({
    name: 'chroma',
    type: 'color',
    inputs: [
      ],
    glsl: `
     float maxrb = max( _c0.r, _c0.b );
     float k = clamp( (_c0.g-maxrb)*5.0, 0.0, 1.0 );
     float dg = _c0.g;
     _c0.g = min( _c0.g, maxrb*0.8 );
     _c0 += vec4(dg - _c0.g);
     return vec4(_c0.rgb, 1.0 - k);
  `})
  osc(60,0.1,1.5).chroma().out(o0)"""
        ]
    , e TODO "synthsettings" "setResolution" 702
        "setResolution( width, height )"
        [ """// make the canvas small (100 pixel x 100 pixel)
setResolution(100,100)
osc().out(o0)"""
        ]
    , e TODO "synthsettings" "speed" 705
        "speed = 1"
        [ """// change overall speed
speed = 3
osc(60,0.1,[0,1.5]).out(o0)", "// change overall speed
speed = 0.1
osc(60,0.1,[0,1.5]).out(o0)"""
        ]
    , e TODO "synthsettings" "time" 709
        "time"
        [ """// default
shape(2,0.8).kaleid(()=>6+Math.sin(time)*4).out(o0)"""
        ]
    , e TODO "synthsettings" "update" 701
        "update(  )"
        [ """// update is called every frame
b = 0
update = () => b += 0.01 * Math.sin(time)
shape().scrollX(()=>b).out(o0)"""
        ]
    , e TODO "synthsettings" "width" 707
        "width"
        [ "shape(99).scrollX(() => -mouse.x / width).out(o0)"
        ]
    ]