module Test.Spec.Hydra.Samples where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.String as String
import Data.Array as Array


import Noodle.Fn.Signature (Signature(..))
import Noodle.Fn.Signature (i, o) as Sig
import Noodle.Text.ToCode (toCode)
import Noodle.Text.FromCode (fromCode, SourceError, srcErrorToString)



import HydraTk.Types
import HydraTk.Repr.Wrap
import HydraTk.Repr.Wrap (WrapRepr(..)) as W
import HydraTk.Types (Ease(..), AudioBin(..), Values(..), GlslFn(..), OnSynth(..), SourceOptions(..), Canvas(..)) as T
import HydraTk.Lang.Command (Command(..), Single(..))
import HydraTk.Lang.Program (Program(..))


textureSamples :: Array Texture
textureSamples =
    [ Empty
    , Start $ From $ Gradient { speed : Number 2.0 }
    , Start $ Load Output3
    , Filter Empty $ Invert Width
    , Filter Empty $ Contrast Height
    ]


programSamples :: Array { program :: Program, jsExpr :: String, pursExpr :: String }
programSamples =
    [
        { program : [ Chain Output0 $ Start $ From $ Noise { scale : Number 10.0, offset : Number 0.1 } ]
        , jsExpr :
          [ "noise( 10.0, 0.1 )"
          , indent ".out( o0 )"
          ]
        , pursExpr : ""
        }
    ,
        { program : [ Chain Output0 $ Start $ From $ Noise { scale : Number 1.5, offset : MouseX } ]
        , jsExpr :
          [ "noise( 1.5, mouse.x )"
          , indent ".out( o0 )"
          ]
        , pursExpr : ""
        }
    ,
        { program : [ Chain Output2 $ Start $ From $ Osc { frequency : Number 60.0, sync : Number 0.1, offset : Number 0.0 } ]
        , jsExpr :
          [ "osc( 60.0, 0.1, 0.0 )"
          , indent ".out( o2 )"
          ]
        , pursExpr : ""
        }
    ,
        { program :
          [ Chain Output2
              $ Geometry
                (Start $ From $ Shape { sides : Number 4.0, radius : Pi, smoothing : Number 2.0 })
              $ GRotate { angle : Number 60.0, speed : Number 2.0 }
          ]
        , jsExpr :
          [ "shape( 4.0, Math.PI, 2.0 )"
          , indent ".rotate( 60.0, 2.0 )"
          , indent ".out( o2 )"
          ]
        , pursExpr : ""
        }
    ,
        { program :
          [ Chain Output2
              $ Filter
                (Start $ From $ Shape { sides : Number 4.0, radius : Pi, smoothing : Number 2.0 })
              $ Contrast $ Number 11.0
          ]
        , jsExpr :
          [ "shape( 4.0, Math.PI, 2.0 )"
          , indent ".contrast( 11.0 )"
          , indent ".out( o2 )"
          ]
        , pursExpr : ""
        }
    ,
        { program :
          [ Chain Output2
              $ BlendOf
                { what : Start $ From $ Noise { scale : Width, offset : Height }
                , with : Start $ From $ Solid { r : Number 1.0, g : Number 0.65, b : Number 0.3, a : Number 0.5 }
                }
              $ Add $ Number 20.0
          ]
        , jsExpr :
          [ "noise( width, height )"
          , indent ".add( solid( 1.0, 0.65, 0.3, 0.5 ), 20.0 )"
          , indent ".out( o2 )"
          ]
        , pursExpr : ""
        }
      , { program :
          [ Chain Output1
              $ ModulateWith
                { what : Start $ From $ Noise { scale : Width, offset : Height }
                , with : Start $ From $ Solid { r : Number 1.0, g : Number 0.65, b : Number 0.3, a : Number 0.5 }
                }
              $ Modulate $ Number 25.0
          ]
        , jsExpr :
          [ "noise( width, height )"
          , indent ".modulate( solid( 1.0, 0.65, 0.3, 0.5 ), 25.0 )"
          , indent ".out( o1 )"
          ]
        , pursExpr : ""
        }
      , { program :
          [ Chain Output0
              $ Filter
                  (Start $ From $ Gradient { speed : Number 0.0 })
              $ Posterize { bins : VArray (T.Values [ Number 1.0, Number 5.0, Number 15.0, Number 30.0, Pi ]) $ T.Ease Linear, gamma : Number 0.5 }
          ]
        , jsExpr :
          [ "gradient( 0.0 )"
          , indent ".posterize( [ 1.0, 5.0, 15.0, 30.0, Math.PI ].ease( 'linear' ), 0.5 )"
          , indent ".out( o0 )"
          ]
        , pursExpr : ""
        }
      , { program :
          [ Single $ WithSynth $ T.SetResolution 100 100
          , Chain Output0
              $ Filter
                  (Start $ From $ Gradient { speed : Fft $ T.AudioBin 2 })
              $ Invert $ Number 2.0
          , Single $ WithSynth $ Hush
          ]
        , jsExpr :
          [ "setResolution( 100, 100 )"
          , empty
          , "gradient( () => a.fft[2] )"
          , indent ".invert( 2.0 )"
          , indent ".out( o0 )"
          , empty
          , "hush()"
          ]
        , pursExpr : ""
        }
      , { program :
          [ Single $ WithSource Source0 $ InitCam 0
          , Chain Output0
              $ Filter
                  (Start $ External Source0)
              $ Invert $ Number 1.0
          ]
        , jsExpr :
          [ "s0.initCam( 0 )"
          , empty
          , "src( s0 )"
          , indent ".invert( 1.0 )"
          , indent ".out( o0 )"
          ]
        , pursExpr : ""
        }
      , { program :
          [ Single $ WithSource Source0 $ Init $ T.SourceOptions { src : T.Canvas }
          , Chain Output0
              $ ModulateWith
                { what : Start $ External Source0
                , with : Geometry (Start $ From $ Osc { frequency : Number 60.0, sync : Number 0.1, offset : Number 0.0 }) $ GKaleid { nSides : Number 999.0 }
                }
              $ Modulate $ Number 0.1
          ]
        , jsExpr :
          [ "s0.init( { src : canvas } )"
          , empty
          , "src( s0 )"
          , indent ".modulate( osc( 60.0, 0.1, 0.0 )"
          , indent $ indent ".kaleid( 999.0 ), 0.1 ) , 0.1 )"
          , indent ".out( o0 )"
          ]
        , pursExpr : ""
        }
      , { program :
          [ Single $ SynthSet $ Speed 3.0
          , Single $ SynthSet $ Bpm 60
          , Single $ WithAudio Mic $ SetScale 20.0
          , Chain Output0 $ Start $ From $ Osc { frequency : Number 60.0, sync : Number 0.1, offset : VArray (T.Values [ Number 0.0, Number 1.5 ]) $ T.Ease InOutCubic }
          , Chain Output1 $ Start $ From $ Osc { frequency : Number 60.0, sync : Number 0.1, offset : VArray (T.Values [ Number 0.0, Number 1.5 ]) $ T.Fast Pi }
          , Single $ WithSynth $ Render Four
          ]
        , jsExpr :
          [ "speed = 3.0"
          , empty
          , "bpm = 60"
          , empty
          , "a.setScale( 20.0 )"
          , empty
          , "osc( 60.0, 0.1, [ 0.0, 1.5 ].ease( 'inOutCubic' ) )"
          , indent ".out( o0 )"
          , empty
          , "osc( 60.0, 0.1, [ 0.0, 1.5 ].fast( Math.PI ) )"
          , indent ".out( o1 )"
          , empty
          , "render( /* FOUR */ )"
          ]
        , pursExpr : ""
        }
    ] <#> \r ->
        { program : Program r.program
        , jsExpr : "/* GENERATED CODE */\n\n" <> String.joinWith "\n" r.jsExpr
        , pursExpr : r.pursExpr
        }
    where
      empty = ""
      indent str = "\t" <> str


-- TODO: use fuzzy generator
wrapSamples :: Array { sample :: WrapRepr, jsExpr :: String, pursExpr :: String }
wrapSamples =
    [ { sample : Unit unit
      , jsExpr : "/* unit */"
      , pursExpr : ""
      }
    , { sample : Value $ Number 2.0
      , jsExpr : "2.0"
      , pursExpr : ""
      }
    , { sample : Value None
      , jsExpr : "null"
      , pursExpr : ""
      }
    , { sample : Value Undefined
      , jsExpr : "undefined"
      , pursExpr : ""
      }
    , { sample : Value Time
      , jsExpr : "time"
      , pursExpr : ""
      }
    , { sample : Value MouseX
      , jsExpr : "mouse.x"
      , pursExpr : ""
      }
    , { sample : Value MouseY
      , jsExpr : "mouse.y"
      , pursExpr : ""
      }
    , { sample : Value Width
      , jsExpr : "width"
      , pursExpr : ""
      }
    , { sample : Value Height
      , jsExpr : "height"
      , pursExpr : ""
      }
    , { sample : Value Pi
      , jsExpr : "pi"
      , pursExpr : ""
      }
    , { sample : Value $ Fft $ T.AudioBin 1
      , jsExpr : "a.fft[1]"
      , pursExpr : ""
      }
    , { sample : Value $ VArray (T.Values []) T.NoEase
      , jsExpr : "[]"
      , pursExpr : ""
      }
    , { sample : Value $ VArray (T.Values []) $ T.Ease Linear
      , jsExpr : "[].ease('linear')"
      , pursExpr : ""
      }
    , { sample : Value $ VArray (T.Values [ MouseX, Time, Pi, Number 4.5, None, Pi ]) $ T.Ease Linear
      , jsExpr : "[ mouse.x, time, 4.5, Math.PI ].ease('linear')"
      , pursExpr : ""
      }
    , { sample : Value $ VArray (T.Values []) $ Fast $ Number 7.0
      , jsExpr : "[].fast(7.0)"
      , pursExpr : ""
      }
    , { sample : Value $ VArray (T.Values []) $ Fit { low : Number 1.0, high : Number 2.5 }
    -- , Value $ Dep $ JsExpr $ Val $ Number 3.0
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture Empty
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Start $ From $ Gradient { speed : Number 2.0 }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Start $ From $ Noise { scale : Number 1.5, offset : MouseX }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Start $ From $ Solid { r : Number 1.5, g : Pi, b : Width, a : Time }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Start $ From $ Voronoi { scale : Width, speed : Pi, blending : MouseX }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Start $ From $ Osc { frequency : Number 60.0, offset : Pi, sync : Width }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Start $ Load Output3
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Start $ External Source0
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Filter Empty $ Invert Width
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Filter Empty $ Contrast Height
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Filter Empty $ Posterize { bins : Time, gamma : Height }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Filter (Start $ Load Output2) $ Shift { r : Number 1.5, g : Pi, b : Width, a : Time }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture $ Filter (Start $ From $ Noise { scale : Number 1.5, offset : MouseX }) $ B { scale : Width, offset : Number 0.0 }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ ModulateWith
            { what : Empty
            , with : Empty
            }
        $ ModHue Pi
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ ModulateWith
            { what : Empty
            , with : Empty
            }
        $ ModKaleid { nSides : Number 7.0 }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ ModulateWith
            { what : Empty
            , with : Start $ From $ Osc { frequency : Number 60.0, offset : Number 0.0, sync : Number 1.0 }
            }
        $ ModKaleid { nSides : Number 7.0 }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ ModulateWith
            { what : Start $ From $ Osc { frequency : Number 60.0, offset : Pi, sync : Number 1.0 }
            , with : Filter (Start $ From $ Noise { scale : Number 1.5, offset : MouseX }) $ B { scale : Width, offset : Number 0.0 }
            }
        $ ModScroll { scrollX : Number 2.0, speedX : Number 1.0, scrollY : Number 0.5, speedY : Pi }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModRepeat { repeatX : Number 2.5, repeatY : Number 1.0, offsetX : Number 5.0, offsetY : Number 2.0 }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModRepeatX { offset : Number 2.5, reps : Number 1.0 }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModScrollY { scrollY : Number 2.5, speed : Number 1.0 }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModScroll { scrollY : Number 2.5, scrollX : Number 1.0, speedY : Number 5.0, speedX : Number 2.0 }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ BlendOf { what : Empty, with : Empty }
        $ Diff
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ BlendOf { what : Start $ Load Output3, with : Filter Empty $ Posterize { bins : Time, gamma : Height } }
        $ Diff
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ BlendOf { what : Empty, with : Empty }
        $ Layer $ Number 2.0
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ BlendOf { what : Empty, with : Empty }
        $ Mult $ Number 0.5
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ Geometry Empty
        $ GKaleid { nSides : Number 7.0 }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ Geometry Empty
        $ GRepeatX { reps : Pi, offset : Number 3.0 }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ Geometry (Start $ Load Output3)
        $ GRepeatY { reps : Number 2.0, offset : Width }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ Geometry (Start $ Load Output1)
        $ GRepeat { repeatX : Number 2.5, repeatY : Number 1.0, offsetX : Number 5.0, offsetY : Number 2.0 }
    {- TODO
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Value
        $ Dep
        $ JsExpr $ AddE (Val $ Number 2.0) (Brackets $ SubE (Math "sin" $ Just $ Val Time) (Val $ Number 0.5))
    -}
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Value $ Dep NoAction
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Value
        $ Dep
        $ Unparsed "foobar"
    {- TODO
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : W.Fn
        $ JsExpr $ AddE (Val $ Number 2.0) (Brackets $ SubE (Math "sin" $ Just $ Val Time) (Val $ Number 0.5))
    -}
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : W.DepFn NoAction
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : W.DepFn
        $ Unparsed "foobar"
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : W.DepFn
        $ Unparsed """
        aaa
        nnn
        bbb282828///
        """
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : W.GlslFn
        $ T.GlslFn
        $ { kind : FnSrc
          , code : GlslFnCode
            """
test
multiline
code
            """
          , fn : Sig ("abc" /\ [] /\ [])
          }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ CallGlslFn { over : Empty, mbWith : Nothing }
        $ GlslFnRef $ Sig ("aaa" /\ [] /\ [])
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ CallGlslFn { over : Empty, mbWith : Nothing }
        $ GlslFnRef $ Sig ("aaa" /\ [ Sig.i "arg1" $ T $ Empty ] /\ [])
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : W.GlslFn
        $ T.GlslFn
        $ { kind : FnSrc
          , code : GlslFnCode "foo\nbar\nbzz"
          , fn : Sig ("axz" /\ [ Sig.i "arg1" $ T $ Empty, Sig.i "arg2" $ V $ Number 2.0 ] /\ [])
          }
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : Texture
        $ CallGlslFn { over : Filter Empty $ Posterize { bins : Time, gamma : Height }, mbWith : Nothing }
        $ GlslFnRef $ Sig
            $ "bzz" /\
                [ Sig.i "a1" $ T Empty
                , Sig.i "a2"
                    $ T $ BlendOf { what : Empty, with : Empty } $ Diff
                , Sig.i "a3" $ V $ Number 2.0
                ] /\
                []
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : W.Target $ Output Output4
      , jsExpr : ""
      , pursExpr : ""
      }
    , { sample : W.Target Four
      , jsExpr : ""
      , pursExpr : ""
      }
    ]