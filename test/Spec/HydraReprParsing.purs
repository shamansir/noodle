module Test.Spec.HydraReprParsing where

import Prelude

import Effect.Console as Console
import Effect.Class (liftEffect)

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions

import Test.Spec.Util.Parsing (parses)
import Test.Spec.Util.Assertions (shouldEqual) as U



import Noodle.Fn.Signature (Signature(..))
import Noodle.Fn.Signature (i, o) as Sig
import Noodle.Text.ToCode (toCode)
import Noodle.Text.FromCode (fromCode, SourceError, srcErrorToString)

import HydraTk.Types
import HydraTk.Repr.Show (hShow)
import HydraTk.Repr.Wrap
import HydraTk.Repr.Wrap (WrapRepr(..)) as W
import HydraTk.Types (Ease(..), AudioBin(..), Values(..), GlslFn(..)) as T
-- import Hydra.Lang.Fn as Lang
import HydraTk.Repr.Target (_encode, _decode) as Hydra

-- import Data.FromToFile (encode, decode)

-- import Toolkit.Hydra.Repr.Wrap.Parser


textureSamples :: Array Texture
textureSamples =
    [ Empty
    , Start $ From $ Gradient { speed : Number 2.0 }
    , Start $ Load Output3
    , Filter Empty $ Invert Width
    , Filter Empty $ Contrast Height
    ]


-- TODO: use fuzzy generator
wrapReprSamples :: Array WrapRepr
wrapReprSamples =
    [ Unit unit
    , Value $ Number 2.0
    , Value None
    , Value Undefined
    , Value Time
    , Value MouseX
    , Value MouseY
    , Value Width
    , Value Height
    , Value Pi
    , Value $ Fft $ T.AudioBin 1
    , Value $ VArray (T.Values []) $ T.Ease Linear
    , Value $ VArray (T.Values [ MouseX, Time, Pi, Number 4.5, None, Pi ]) $ T.Ease Linear
    , Value $ VArray (T.Values []) $ Fast $ Number 7.0
    , Value $ VArray (T.Values []) $ Fit { low : Number 1.0, high : Number 2.5 }
    -- , Value $ Dep $ JsExpr $ Val $ Number 3.0
    , Texture Empty
    , Texture $ Start $ From $ Gradient { speed : Number 2.0 }
    , Texture $ Start $ From $ Noise { scale : Number 1.5, offset : MouseX }
    , Texture $ Start $ From $ Solid { r : Number 1.5, g : Pi, b : Width, a : Time }
    , Texture $ Start $ From $ Voronoi { scale : Width, speed : Pi, blending : MouseX }
    , Texture $ Start $ From $ Osc { frequency : Number 60.0, offset : Pi, sync : Width }
    , Texture $ Start $ Load Output3
    , Texture $ Start $ External Source0 $ Camera 2
    , Texture $ Start $ External Source0 Video
    , Texture $ Start $ External Source0 $ Sketch "foobar"
    , Texture $ Filter Empty $ Invert Width
    , Texture $ Filter Empty $ Contrast Height
    , Texture $ Filter Empty $ Posterize { bins : Time, gamma : Height }
    , Texture $ Filter (Start $ Load Output2) $ Shift { r : Number 1.5, g : Pi, b : Width, a : Time }
    , Texture $ Filter (Start $ From $ Noise { scale : Number 1.5, offset : MouseX }) $ B { scale : Width, offset : Number 0.0 }
    , Texture
        $ ModulateWith
            { what : Empty
            , with : Empty
            }
        $ ModHue Pi
    , Texture
        $ ModulateWith
            { what : Empty
            , with : Empty
            }
        $ ModKaleid { nSides : Number 7.0 }
    , Texture
        $ ModulateWith
            { what : Empty
            , with : Start $ From $ Osc { frequency : Number 60.0, offset : Number 0.0, sync : Number 1.0 }
            }
        $ ModKaleid { nSides : Number 7.0 }
    , Texture
        $ ModulateWith
            { what : Start $ From $ Osc { frequency : Number 60.0, offset : Pi, sync : Number 1.0 }
            , with : Filter (Start $ From $ Noise { scale : Number 1.5, offset : MouseX }) $ B { scale : Width, offset : Number 0.0 }
            }
        $ ModScroll { scrollX : Number 2.0, speedX : Number 1.0, scrollY : Number 0.5, speedY : Pi }
    , Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModRepeat { repeatX : Number 2.5, repeatY : Number 1.0, offsetX : Number 5.0, offsetY : Number 2.0 }
    , Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModRepeatX { offset : Number 2.5, reps : Number 1.0 }
    , Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModScrollY { scrollY : Number 2.5, speed : Number 1.0 }
    , Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModScroll { scrollY : Number 2.5, scrollX : Number 1.0, speedY : Number 5.0, speedX : Number 2.0 }
    , Texture
        $ BlendOf { what : Empty, with : Empty }
        $ Diff
    , Texture
        $ BlendOf { what : Start $ Load Output3, with : Filter Empty $ Posterize { bins : Time, gamma : Height } }
        $ Diff
    , Texture
        $ BlendOf { what : Empty, with : Empty }
        $ Layer $ Number 2.0
    , Texture
        $ BlendOf { what : Empty, with : Empty }
        $ Mult $ Number 0.5
    , Texture
        $ Geometry Empty
        $ GKaleid { nSides : Number 7.0 }
    , Texture
        $ Geometry Empty
        $ GRepeatX { reps : Pi, offset : Number 3.0 }
    , Texture
        $ Geometry (Start $ Load Output3)
        $ GRepeatY { reps : Number 2.0, offset : Width }
    , Texture
        $ Geometry (Start $ Load Output1)
        $ GRepeat { repeatX : Number 2.5, repeatY : Number 1.0, offsetX : Number 5.0, offsetY : Number 2.0 }
    {- TODO
    , Value
        $ Dep
        $ JsExpr $ AddE (Val $ Number 2.0) (Brackets $ SubE (Math "sin" $ Just $ Val Time) (Val $ Number 0.5))
    -}
    , Value $ Dep NoAction
    , Value
        $ Dep
        $ Unparsed "foobar"
    {- TODO
    , W.Fn
        $ JsExpr $ AddE (Val $ Number 2.0) (Brackets $ SubE (Math "sin" $ Just $ Val Time) (Val $ Number 0.5))
    -}
    , W.DepFn NoAction
    , W.DepFn
        $ Unparsed "foobar"
    , W.DepFn
        $ Unparsed """
        aaa
        nnn
        bbb282828///
        """
    , W.GlslFn
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
    , Texture
        $ CallGlslFn { over : Empty, mbWith : Nothing }
        $ GlslFnRef $ Sig ("aaa" /\ [] /\ [])
    , Texture
        $ CallGlslFn { over : Empty, mbWith : Nothing }
        $ GlslFnRef $ Sig ("aaa" /\ [ Sig.i "arg1" $ T $ Empty ] /\ [])
    , W.GlslFn
        $ T.GlslFn
        $ { kind : FnSrc
          , code : GlslFnCode "foo\nbar\nbzz"
          , fn : Sig ("axz" /\ [ Sig.i "arg1" $ T $ Empty, Sig.i "arg2" $ V $ Number 2.0 ] /\ [])
          }
    , Texture
        $ CallGlslFn { over : Filter Empty $ Posterize { bins : Time, gamma : Height }, mbWith : Nothing }
        $ GlslFnRef $ Sig
            $ "bzz" /\
                [ Sig.i "a1" $ T Empty
                , Sig.i "a2"
                    $ T $ BlendOf { what : Empty, with : Empty } $ Diff
                , Sig.i "a3" $ V $ Number 2.0
                ] /\
                []
    ]


spec :: Spec Unit
spec = do

  describe "Works for all Wrap Repr samples" $ do

    foldlWithIndex
        (\idx prev sample -> do
            prev
            *>
            (it ("works for sample " <> show idx <> " : " <> hShow sample <> " : " <> Hydra._encode sample) $
                case (Hydra._decode $ Hydra._encode sample :: Either SourceError WrapRepr) of
                    Right decoded -> do
                        -- liftEffect $ Console.log $ Hydra._encode sample
                        (Hydra._encode decoded) `U.shouldEqual` (Hydra._encode sample)
                    Left srcError -> fail $ "\t" <> Hydra._encode sample <> "\n\n\tfailed to decode the following sample:\n\n\t" <> hShow sample <> "\n\n\t" <> srcErrorToString srcError
            )
        )
        (pure unit)
        wrapReprSamples

  describe "Works for all textures" $ do

    foldlWithIndex
        (\idx prev sample -> do
            prev
            *>
            (it ("works for sample " <> show idx <> " : " <> hShow sample {- <> " : " <> Hydra._encode sample-}) $
                case (Hydra._decode $ Hydra._encode sample :: Either SourceError Texture) of
                    Right decoded ->
                        (Hydra._encode decoded) `U.shouldEqual` (Hydra._encode sample)
                    Left srcError -> fail $ "\t" <> Hydra._encode sample <> "\n\n\tfailed to decode the following sample:\n\n\t" <> hShow sample <> "\n\n\t" <> srcErrorToString srcError
            )
        )
        (pure unit)
        textureSamples