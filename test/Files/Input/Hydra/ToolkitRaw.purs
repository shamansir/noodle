module INPUT.Hydra.Gen.ToolkitRaw where

import Prelude
import Effect.Class (class MonadEffect)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Type.Data.List.Extra (TNil)
import Noodle.Id (toolkitR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (empty) as Toolkit
import Noodle.Unsafe.QuickMake.RawToolkit (qregister) as Toolkit
import HydraTk.Repr.State (StateRepr)
import HydraTk.Repr.Wrap (WrapRepr)
import HydraTk.Patch (PState(..))
import HydraTk.Patch (init) as Patch
import HydraTk.Types as HYDRA
import HydraTk.Repr.Wrap as HYDRAW
import Data.Tuple.Nested ((/\))

foreign import data HYDRA :: ToolkitKey

toolkit :: forall m. MonadEffect m => Toolkit HYDRA TNil StateRepr WrapRepr m
toolkit = Toolkit.empty (Proxy :: _ HYDRA) (Id.toolkitR "Hydra")
  #
    ( {- number -} Toolkit.qregister "number" []
        [ { name: "out", tag: "Value", value: Just "N 0.0" } ] $ pure unit
    )
  #
    ( {- pi -} Toolkit.qregister "pi" [] [ { name: "out", tag: "Value", value: Just "PI V" } ] $ pure
        unit
    )
  #
    ( {- array -} Toolkit.qregister "array" []
        [ { name: "out", tag: "Value", value: Just "VA %%%%" } ] $ pure unit
    )
  #
    ( {- expression -} Toolkit.qregister "expression" []
        [ { name: "out", tag: "Value", value: Just "D /----/" } ] $ pure unit
    )
  #
    ( {- time -} Toolkit.qregister "time" [] [ { name: "time", tag: "Value", value: Just "T V" } ] $
        pure unit
    )
  #
    ( {- mouse -} Toolkit.qregister "mouse" []
        [ { name: "x", tag: "Value", value: Just "MX V" }
        , { name: "y", tag: "Value", value: Just "MY V" }
        ] $ pure unit
    )
  #
    ( {- glslFn -} Toolkit.qregister "glslFn" []
        [ { name: "out", tag: "Value", value: Just "D /----/" } ] $ pure unit
    )
  #
    ( {- noise -} Toolkit.qregister "noise"
        [ { name: "scale", tag: "Value", value: Just "N 10.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.1" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- voronoi -} Toolkit.qregister "voronoi"
        [ { name: "scale", tag: "Value", value: Just "N 5.0" }
        , { name: "speed", tag: "Value", value: Just "N 0.3" }
        , { name: "blending", tag: "Value", value: Just "N 0.3" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- osc -} Toolkit.qregister "osc"
        [ { name: "frequency", tag: "Value", value: Just "N 60.0" }
        , { name: "sync", tag: "Value", value: Just "N 0.1" }
        , { name: "offset", tag: "Value", value: Nothing }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- shape -} Toolkit.qregister "shape"
        [ { name: "sides", tag: "Value", value: Just "N 60.0" }
        , { name: "radius", tag: "Value", value: Just "N 0.3" }
        , { name: "smoothing", tag: "Value", value: Just "N 0.01" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- gradient -} Toolkit.qregister "gradient" [ { name: "speed", tag: "Value", value: Nothing } ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- src -} Toolkit.qregister "src" [ { name: "load", tag: "OutputN", value: Nothing } ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- solid -} Toolkit.qregister "solid"
        [ { name: "r", tag: "Value", value: Nothing }
        , { name: "g", tag: "Value", value: Nothing }
        , { name: "b", tag: "Value", value: Nothing }
        , { name: "a", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- prev -} Toolkit.qregister "prev" [ { name: "todo", tag: "TODO", value: Just "TODO" } ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- rotate -} Toolkit.qregister "rotate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "angle", tag: "Value", value: Just "N 10.0" }
        , { name: "speed", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- scale -} Toolkit.qregister "scale"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.5" }
        , { name: "xMult", tag: "Value", value: Just "N 1.0" }
        , { name: "yMult", tag: "Value", value: Just "N 1.0" }
        , { name: "offsetX", tag: "Value", value: Just "N 0.5" }
        , { name: "offsetY", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- pixelate -} Toolkit.qregister "pixelate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "pixelX", tag: "Value", value: Just "N 20.0" }
        , { name: "pixelY", tag: "Value", value: Just "N 20.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- repeat -} Toolkit.qregister "repeat"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "repeatX", tag: "Value", value: Just "N 3.0" }
        , { name: "repeatY", tag: "Value", value: Just "N 3.0" }
        , { name: "offsetX", tag: "Value", value: Just "N 0.0" }
        , { name: "offsetY", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- repeatX -} Toolkit.qregister "repeatX"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "reps", tag: "Value", value: Just "N 3.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- repeatY -} Toolkit.qregister "repeatY"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "reps", tag: "Value", value: Just "N 3.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- kaleid -} Toolkit.qregister "kaleid"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "nSides", tag: "Value", value: Just "N 3.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- scroll -} Toolkit.qregister "scroll"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scrollX", tag: "Value", value: Just "N 0.5" }
        , { name: "scrollY", tag: "Value", value: Just "N 0.5" }
        , { name: "speedX", tag: "Value", value: Nothing }
        , { name: "speedY", tag: "Value", value: Nothing }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- scrollX -} Toolkit.qregister "scrollX"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scrollX", tag: "Value", value: Just "N 0.5" }
        , { name: "speed", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- scrollY -} Toolkit.qregister "scrollY"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scrollY", tag: "Value", value: Just "N 0.5" }
        , { name: "speed", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- posterize -} Toolkit.qregister "posterize"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "bins", tag: "Value", value: Just "N 3.0" }
        , { name: "gamma", tag: "Value", value: Just "N 0.6" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- shift -} Toolkit.qregister "shift"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "r", tag: "Value", value: Just "N 0.5" }
        , { name: "g", tag: "Value", value: Just "N 0.5" }
        , { name: "b", tag: "Value", value: Just "N 0.5" }
        , { name: "a", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- invert -} Toolkit.qregister "invert"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- contrast -} Toolkit.qregister "contrast"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.6" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- brightness -} Toolkit.qregister "brightness"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 0.4" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- luma -} Toolkit.qregister "luma"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "threshold", tag: "Value", value: Just "N 0.5" }
        , { name: "tolerance", tag: "Value", value: Just "N 0.1" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- thresh -} Toolkit.qregister "thresh"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "threshold", tag: "Value", value: Just "N 0.5" }
        , { name: "tolerance", tag: "Value", value: Just "N 0.1" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- color -} Toolkit.qregister "color"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "r", tag: "Value", value: Just "N 1.0" }
        , { name: "g", tag: "Value", value: Just "N 1.0" }
        , { name: "b", tag: "Value", value: Just "N 1.0" }
        , { name: "a", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- saturate -} Toolkit.qregister "saturate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 2.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- hue -} Toolkit.qregister "hue"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "hue", tag: "Value", value: Just "N 0.4" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- colorama -} Toolkit.qregister "colorama"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 0.005" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- sum -} Toolkit.qregister "sum"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "todo", tag: "TODO", value: Just "TODO" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- r -} Toolkit.qregister "r"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scale", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- b -} Toolkit.qregister "b"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scale", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- g -} Toolkit.qregister "g"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scale", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- a -} Toolkit.qregister "a"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scale", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- add -} Toolkit.qregister "add"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- sub -} Toolkit.qregister "sub"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- layer -} Toolkit.qregister "layer"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- blend -} Toolkit.qregister "blend"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- mult -} Toolkit.qregister "mult"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- diff -} Toolkit.qregister "diff"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- mask -} Toolkit.qregister "mask"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- modulateRepeat -} Toolkit.qregister "modulateRepeat"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "repeatX", tag: "Value", value: Just "N 3.0" }
        , { name: "repeatY", tag: "Value", value: Just "N 3.0" }
        , { name: "offsetX", tag: "Value", value: Just "N 0.5" }
        , { name: "offsetY", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- modulateRepeatX -} Toolkit.qregister "modulateRepeatX"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "reps", tag: "Value", value: Just "N 3.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- modulateRepeatY -} Toolkit.qregister "modulateRepeatY"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "reps", tag: "Value", value: Just "N 3.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- modulateKaleid -} Toolkit.qregister "modulateKaleid"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "nSides", tag: "Value", value: Just "N 3.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- modulateScrollX -} Toolkit.qregister "modulateScrollX"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "scrollX", tag: "Value", value: Just "N 0.5" }
        , { name: "speed", tag: "Value", value: Nothing }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- modulateScrollY -} Toolkit.qregister "modulateScrollY"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "scrollY", tag: "Value", value: Just "N 0.5" }
        , { name: "speed", tag: "Value", value: Nothing }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- modulate -} Toolkit.qregister "modulate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 0.1" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- modulateScale -} Toolkit.qregister "modulateScale"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "multiple", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- modulatePixelate -} Toolkit.qregister "modulatePixelate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "multiple", tag: "Value", value: Just "N 10.0" }
        , { name: "offset", tag: "Value", value: Just "N 3.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- modulateRotate -} Toolkit.qregister "modulateRotate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "multiple", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Nothing }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- modulateHue -} Toolkit.qregister "modulateHue"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ pure unit
    )
  #
    ( {- initCam -} Toolkit.qregister "initCam"
        [ { name: "src", tag: "SourceN", value: Nothing }
        , { name: "index", tag: "Value", value: Just "0 V" }
        ]
        [] $ pure unit
    )
  #
    ( {- initImage -} Toolkit.qregister "initImage"
        [ { name: "src", tag: "SourceN", value: Nothing }
        , { name: "url", tag: "Url", value: Nothing }
        ]
        [] $ pure unit
    )
  #
    ( {- initVideo -} Toolkit.qregister "initVideo"
        [ { name: "src", tag: "SourceN", value: Nothing }
        , { name: "url", tag: "Url", value: Nothing }
        ]
        [] $ pure unit
    )
  #
    ( {- init -} Toolkit.qregister "init"
        [ { name: "options", tag: "SourceOptions", value: Nothing } ]
        [] $ pure unit
    )
  #
    ( {- initStream -} Toolkit.qregister "initStream"
        [ { name: "src", tag: "SourceN", value: Nothing }
        , { name: "todo", tag: "TODO", value: Just "TODO" }
        ]
        [] $ pure unit
    )
  # ( {- initScreen -} Toolkit.qregister "initScreen" [] [] $ pure unit)
  #
    ( {- render -} Toolkit.qregister "render"
        [ { name: "what", tag: "RenderTarget", value: Just "ALL 4" } ]
        [] $ pure unit
    )
  #
    ( {- update -} Toolkit.qregister "update" [ { name: "fn", tag: "UpdateFn", value: Nothing } ] []
        $ pure unit
    )
  #
    ( {- setResolution -} Toolkit.qregister "setResolution"
        [ { name: "width", tag: "Value", value: Just "N 100.0" }
        , { name: "height", tag: "Value", value: Just "N 100.0" }
        ]
        [] $ pure unit
    )
  # ( {- hush -} Toolkit.qregister "hush" [] [] $ pure unit)
  #
    ( {- setFunction -} Toolkit.qregister "setFunction"
        [ { name: "fn", tag: "GlslFn", value: Nothing } ]
        [] $ pure unit
    )
  #
    ( {- speed -} Toolkit.qregister "speed" [ { name: "v", tag: "Value", value: Just "N 1.0" } ] [] $
        pure unit
    )
  #
    ( {- bpm -} Toolkit.qregister "bpm" [ { name: "v", tag: "Value", value: Just "N 30.0" } ] [] $
        pure unit
    )
  #
    ( {- width -} Toolkit.qregister "width" [] [ { name: "w", tag: "Value", value: Just "W V" } ] $
        pure unit
    )
  #
    ( {- height -} Toolkit.qregister "height" [] [ { name: "h", tag: "Value", value: Just "H V" } ] $
        pure unit
    )
  #
    ( {- fast -} Toolkit.qregister "fast"
        [ { name: "arr", tag: "Values", value: Just "%%%%" }
        , { name: "speed", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "arr", tag: "Value", value: Just "VA %%%%" } ] $ pure unit
    )
  #
    ( {- smooth -} Toolkit.qregister "smooth"
        [ { name: "arr", tag: "Values", value: Just "%%%%" }
        , { name: "smooth", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "arr", tag: "Value", value: Just "VA %%%%" } ] $ pure unit
    )
  #
    ( {- ease -} Toolkit.qregister "ease"
        [ { name: "arr", tag: "Values", value: Just "%%%%" }
        , { name: "ease", tag: "Ease", value: Just "LIN E" }
        ]
        [ { name: "arr", tag: "Value", value: Just "VA %%%%" } ] $ pure unit
    )
  #
    ( {- offset -} Toolkit.qregister "offset"
        [ { name: "arr", tag: "Values", value: Just "%%%%" }
        , { name: "offset", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "arr", tag: "Value", value: Just "VA %%%%" } ] $ pure unit
    )
  #
    ( {- fit -} Toolkit.qregister "fit"
        [ { name: "arr", tag: "Values", value: Just "%%%%" }
        , { name: "low", tag: "Value", value: Just "N 0.0" }
        , { name: "high", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "arr", tag: "Value", value: Just "VA %%%%" } ] $ pure unit
    )
  #
    ( {- fft -} Toolkit.qregister "fft" [ { name: "bin", tag: "AudioBin", value: Just "@0" } ]
        [ { name: "fft", tag: "Value", value: Just "0 V" } ] $ pure unit
    )
  #
    ( {- setSmooth -} Toolkit.qregister "setSmooth"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "smooth", tag: "Value", value: Just "N 0.4" }
        ]
        [] $ pure unit
    )
  #
    ( {- setCutoff -} Toolkit.qregister "setCutoff"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "cutoff", tag: "Value", value: Just "N 2.0" }
        ]
        [] $ pure unit
    )
  #
    ( {- setBins -} Toolkit.qregister "setBins"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "numBins", tag: "Value", value: Just "N 4.0" }
        ]
        [] $ pure unit
    )
  #
    ( {- setScale -} Toolkit.qregister "setScale"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "scale", tag: "Value", value: Just "N 10.0" }
        ]
        [] $ pure unit
    )
  #
    ( {- hide -} Toolkit.qregister "hide"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "todo", tag: "TODO", value: Just "TODO" }
        ]
        [] $ pure unit
    )
  #
    ( {- show -} Toolkit.qregister "show"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "todo", tag: "TODO", value: Just "TODO" }
        ]
        [] $ pure unit
    )
  #
    ( {- out -} Toolkit.qregister "out"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "target", tag: "RenderTarget", value: Just "ALL 4" }
        ]
        [] $ pure unit
    )
