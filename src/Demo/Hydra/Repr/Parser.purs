module Hydra.Repr.Parser where
-- TODO: may be move value parsers to Hydra.Lang, but these parsers here encode NDF file structure, they are different from SketchParser,
-- but JsExpr parser could be separated since it is the same everywhere

import Prelude

import Parsing.Extra (marker, foldMarkers, parseBy)

import Control.Plus (empty)
import Control.Lazy (defer)
import Control.Alt ((<|>))

import Data.Foldable (foldr)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (fromFoldable, length) as Array
import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Either (hush) as Either
import Data.String.Read (class Read)

import Parsing (Parser, runParser, fail)
import Parsing.String (string)
import Parsing.Combinators (try, many1, sepBy1, between, sepBy, replicateA, many1Till, optional, optionMaybe)
import Parsing.String.Basic (space, number, intDecimal, alphaNum, takeWhile1, noneOf)
import Parsing.String.Extra as U
import Parsing.Extra (class HasParser, parser, read)
import Parsing.Expr (buildExprParser, Assoc(..), Operator(..))


-- import Hydra.Repr.Wrap (WrapRepr(..))

-- import Hydra.Lang.SketchParser.Utils as U
-- import Noodle.Fn.ToFn (class ToFn, toFn, class PossiblyToFn, possiblyToFn, q, o)
import Noodle.Fn.Signature (Signature, sigOf) as Lang
import Noodle.Fn.Signature (empty, argsCount, out1) as Sig


import Hydra.Types as HT
import Hydra.Repr.Markers as PM


value :: Parser String HT.Value
value =
    foldMarkers
        [ marker $ "N" /\ HT.Number /\ number
        , marker $ "0" /\ const HT.None /\ string "V"
        , marker $ "U" /\ const HT.Undefined /\ string "V"
        , marker $ "T" /\ const HT.Time /\ string "V"
        , marker $ "MX" /\ const HT.MouseX /\ string "V"
        , marker $ "MY" /\ const HT.MouseY /\ string "V"
        , marker $ "W" /\ const HT.Width /\ string "V"
        , marker $ "H" /\ const HT.Height /\ string "V"
        , marker $ "PI" /\ const HT.Pi /\ string "V"
        , marker $ "A" /\ HT.Fft /\ audioBin
        , marker $ "VA" /\ uncurry HT.VArray /\
                    ((/\)
                        <$> (defer \_ -> values)
                        <*> (fromMaybe HT.Linear <$> optionMaybe (string " $$ " *> defer \_ -> ease))
                    )
        , marker $ "D" /\ HT.Dep /\ fn
        ]


audioBin :: Parser String HT.AudioBin
audioBin = HT.AudioBin <$> (string "@" *> intDecimal)


values :: Parser String HT.Values
values =
    string PM._arrEmpty *> pure (HT.Values [])
    <|> HT.Values
        <$> Array.fromFoldable
        <$>
        (between (string PM._arrStart) (string PM._arrEnd)
            $ flip sepBy (string PM._arrSep) $ defer $ \f -> value
        )


outputN :: Parser String HT.OutputN
outputN =
    string "O" *> intDecimal <#> case _ of
        0 -> HT.Output0
        1 -> HT.Output1
        2 -> HT.Output2
        3 -> HT.Output3
        4 -> HT.Output4
        _ -> HT.Output0


sourceN :: Parser String HT.SourceN
sourceN =
    string "S" *> intDecimal <#> const HT.Source0


todo :: Parser String HT.TODO
todo = pure HT.TODO


texture :: Parser String HT.Texture
texture =
    foldMarkers
        [ marker $ "EMP" /\ const HT.Empty /\ string "T"
        , marker $ "S" /\ HT.Start /\ source
        , marker $ "F" /\ uncurry HT.Filter /\ do
            tex <- defer \_ -> texture
            _ <- string PM._texSep
            cop <- colorOp
            _ <- string PM._texsEnd
            pure $ tex /\ cop
        , marker $ "M" /\ uncurry HT.ModulateWith /\ do
            what <- defer \_ -> texture
            _ <- string PM._texSep
            with <- defer \_ -> texture
            _ <- string PM._texSep
            mod <- modulate
            _ <- string PM._texsEnd
            pure $ { what, with } /\ mod
        , marker $ "B" /\ uncurry HT.BlendOf /\ do
            what <- defer \_ -> texture
            _ <- string PM._texSep
            with <- defer \_ -> texture
            _ <- string PM._texSep
            bl <- blend
            _ <- string PM._texsEnd
            pure $ { what, with } /\ bl
        , marker $ "G" /\ uncurry HT.Geometry /\ do
            tex <- defer \_ -> texture
            _ <- string PM._texSep
            geo <- geometry
            _ <- string PM._texsEnd
            pure $ tex /\ geo
        , marker $ "CALL" /\ uncurry HT.CallGlslFn /\ do
            over <- defer \_ -> texture
            _ <- string PM._texSep
            mbWith <- optionMaybe $ try $ do
                with <- defer \_ -> texture
                _ <- string PM._texSep
                pure with
            fn <- langFn tOrV
            -- _ <- if Fn.argsCount fn > 0 then string PM._texsEnd else
            -- _ <- optional space
            -- _ <- string HT.argsEnd
            -- _ <- string PM._texsEnd
            _ <- optional $ try $ string PM._texsEnd
            pure $ { over, mbWith } /\ HT.GlslFnRef fn
        ]


source :: Parser String HT.Source
source =
    foldMarkers
        [ marker $ "S" /\ (HT.From <<< HT.Solid) /\ parseArgs4V \r g b a -> { r, g, b, a }
        , marker $ "G" /\ (HT.From <<< HT.Gradient) /\ parseArgs1V \speed -> { speed }
        , marker $ "N" /\ (HT.From <<< HT.Noise) /\ parseArgs2V \scale offset -> { scale, offset }
        , marker $ "OSC" /\ (HT.From <<< HT.Osc) /\ parseArgs3V \frequency sync offset -> { frequency, sync, offset }
        , marker $ "SCP" /\ (HT.From <<< HT.Shape) /\ parseArgs3V \sides radius smoothing -> { sides, radius, smoothing }
        , marker $ "V" /\ (HT.From <<< HT.Voronoi) /\ parseArgs3V \scale speed blending -> { scale, speed, blending }
        , marker $ "O" /\ HT.Load /\ outputN
        , marker $ "X" /\ uncurry HT.External /\ do
            src <- sourceN
            _ <- string PM._argSep
            def <- extSource
            _ <- string PM._argsEnd
            pure $ src /\ def

        ]


extSource :: Parser String HT.ExtSource
extSource =
    foldMarkers
        [ marker $ "C" /\ HT.Camera /\ intDecimal
        , marker $ "SK" /\ HT.Sketch /\ (U.f1ts <$> many1 alphaNum)
        , marker $ "V" /\ const HT.Video /\ string "X"
        , marker $ "U" /\ const HT.Unclear /\ string "X"
        ]


ease :: Parser String HT.Ease
ease = --pure HT.Linear
    foldMarkers
        [ marker $ "LIN" /\ const HT.Linear /\ string "E"
        , marker $ "FST" /\ HT.Fast /\ defer \_ -> value
        , marker $ "SMT" /\ HT.Smooth /\ defer \_ -> value
        , marker $ "OFF" /\ HT.Offset /\ defer \_ -> value
        , marker $ "IOC" /\ const HT.InOutCubic /\ string "E"
        , marker $ "FIT" /\ fit /\ do
                                low <- value
                                _ <- string " < "
                                high <- value
                                pure $ low /\ high

        ]
    where
        fit (low /\ high) = HT.Fit { low, high }


blend :: Parser String HT.Blend
blend =
    foldMarkers
        [ marker $ "BLEND" /\ HT.Blend /\ parseArgs1V identity
        , marker $ "ADD" /\ HT.Add /\ parseArgs1V identity
        , marker $ "LAYER" /\ HT.Layer /\ parseArgs1V identity
        , marker $ "MULT" /\ HT.Mult /\ parseArgs1V identity
        , marker $ "SUB" /\ HT.Sub /\ parseArgs1V identity
        , marker $ "DIFF" /\ const HT.Diff /\ noArgs unit
        , marker $ "MASK" /\ const HT.Mask /\ noArgs unit
        ]


colorOp :: Parser String HT.ColorOp
colorOp =
    foldMarkers
        [ marker $ "R" /\ HT.R /\ parseArgs2V \scale offset -> { scale, offset }
        , marker $ "G" /\ HT.G /\ parseArgs2V \scale offset -> { scale, offset }
        , marker $ "B" /\ HT.B /\ parseArgs2V \scale offset -> { scale, offset }
        , marker $ "A" /\ HT.A /\ parseArgs2V \scale offset -> { scale, offset }
        , marker $ "POSTERIZE" /\ HT.Posterize /\ parseArgs2V \bins gamma -> { bins, gamma }
        , marker $ "SHIFT" /\ HT.Shift /\ parseArgs4V \r g b a -> { r, g, b, a }
        , marker $ "COLOR" /\ HT.Color /\ parseArgs4V \r g b a -> { r, g, b, a }
        , marker $ "LUMA" /\ HT.Luma /\ parseArgs2V \threshold tolerance -> { threshold, tolerance }
        , marker $ "TRESH" /\ HT.Thresh /\ parseArgs2V \threshold tolerance -> { threshold, tolerance }
        , marker $ "INVERT" /\ HT.Invert /\ parseArgs1V identity
        , marker $ "CONTRAST" /\ HT.Contrast /\ parseArgs1V identity
        , marker $ "BRIGHTNESS" /\ HT.Brightness /\ parseArgs1V identity
        , marker $ "SATURATE" /\ HT.Saturate /\ parseArgs1V identity
        , marker $ "HUE" /\ HT.Brightness /\ parseArgs1V identity
        , marker $ "COLORAMA" /\ HT.Saturate /\ parseArgs1V identity
        ]


modulate :: Parser String HT.Modulate
modulate =
    foldMarkers
        [ marker $ "MODHUE" /\ HT.ModHue /\ parseArgs1V identity
        , marker $ "MODULATE" /\ HT.Modulate /\ parseArgs1V identity
        , marker $ "MODKALEID" /\ HT.ModKaleid /\ parseArgs1V \nSides -> { nSides }
        , marker $ "MODPIXELATE" /\ HT.ModPixelate /\ parseArgs2V \multiple offset -> { multiple, offset }
        , marker $ "MODREPEATX" /\ HT.ModRepeatX /\ parseArgs2V \reps offset -> { reps, offset }
        , marker $ "MODREPEATY" /\ HT.ModRepeatY /\ parseArgs2V \reps offset -> { reps, offset }
        , marker $ "MODREPEAT" /\ HT.ModRepeat /\ parseArgs4V \repeatX repeatY offsetX offsetY -> { repeatX, repeatY, offsetX, offsetY }
        , marker $ "MODROTATE" /\ HT.ModRotate /\ parseArgs2V \multiple offset ->  { multiple, offset }
        , marker $ "MODSCALE" /\ HT.ModScale /\ parseArgs2V \multiple offset ->  { multiple, offset }
        , marker $ "MODSCROLLX" /\ HT.ModScrollX /\ parseArgs2V \scrollX speed -> { scrollX, speed }
        , marker $ "MODSCROLLY" /\ HT.ModScrollY /\ parseArgs2V \scrollY speed -> { scrollY, speed }
        , marker $ "MODSCROLL" /\ HT.ModScroll /\ parseArgs4V \scrollX scrollY speedX speedY -> { scrollX, scrollY, speedX, speedY }
        ]


geometry :: Parser String HT.Geometry
geometry =
    foldMarkers
        [ marker $ "KALEID" /\ HT.GKaleid /\ parseArgs1V \nSides -> { nSides }
        , marker $ "PIXELATE" /\ HT.GPixelate /\ parseArgs2V \pixelX pixelY -> { pixelX, pixelY }
        , marker $ "REPEATX" /\ HT.GRepeatX /\ parseArgs2V \reps offset -> { reps, offset }
        , marker $ "REPEATY" /\ HT.GRepeatY /\ parseArgs2V \reps offset -> { reps, offset }
        , marker $ "REPEAT" /\ HT.GRepeat /\ parseArgs4V \repeatX repeatY offsetX offsetY -> { repeatX, repeatY, offsetX, offsetY }
        , marker $ "ROTATE" /\ HT.GRotate /\ parseArgs2V \angle speed ->  { angle, speed }
        , marker $ "SCALE" /\ HT.GScale /\ parseArgs5V \amount xMult yMult offsetX offsetY ->  { amount, xMult, yMult, offsetX, offsetY }
        , marker $ "SCROLLX" /\ HT.GScrollX /\ parseArgs2V \scrollX speed -> { scrollX, speed }
        , marker $ "SCROLLY" /\ HT.GScrollY /\ parseArgs2V \scrollY speed -> { scrollY, speed }
        , marker $ "SCROLL" /\ HT.GScroll /\ parseArgs4V \scrollX scrollY speedX speedY -> { scrollX, scrollY, speedX, speedY }
        ]


audioSrc :: Parser String HT.AudioSource
audioSrc =
        string "SIL" $> HT.Silence
    <|> string "MIC" $> HT.Mic
    <|> string "FIL" $> HT.File


renderTarget :: Parser String HT.RenderTarget
renderTarget =
    foldMarkers
        [ marker $ "ALL" /\ const HT.Four /\ string "4"
        , marker $ "O" /\ HT.Output /\ args1 outputN identity
        ]


glslKind :: Parser String HT.GlslFnKind
glslKind =
        string "SRC" $> HT.FnSrc
    <|> string "CRD" $> HT.FnCoord
    <|> string "CCR" $> HT.FnCombineCoord
    <|> string "CMB" $> HT.FnCombine
    <|> string "CLR" $> HT.FnColor


glslCode :: Parser String HT.GlslFnCode
glslCode =
    between (string PM._glslStart) (string PM._glslEnd) (many1 $ noneOf PM._glslTerminals)
        <#> U.f1ts
        <#> HT.GlslFnCode


glsl :: Parser String HT.GlslFn
glsl = do
    kind <- glslKind
    _ <- space
    code <- glslCode
    _ <- space
    gfn <- langFn tOrV
    pure $ HT.GlslFn  { kind, code, fn : gfn }


langFn ::forall x. Parser String x -> Parser String (Lang.Signature x Unit)
langFn argParser = do
    name <- many1 alphaNum
    _ <- space
    argsN <- intDecimal
    _ <- space
    args <- parseNamedArgsHelper argParser argsN (identity >>> Just)
    -- args <- if (argsN > 0) then
    --             space *> parseNamedArgsHelper argParser argsN (identity >>> Just)
    --         else pure []
    pure $ Lang.sigOf (U.f1ts name) args # Sig.out1 ("out" /\ unit)


tOrV :: Parser String HT.TOrV
tOrV =
    foldMarkers
        [ marker $ "TT" /\ HT.T /\ defer \_ -> texture
        , marker $ "VV" /\ HT.V /\ value
        ]


fn :: Parser String HT.DepFn
fn =
    HT.NoAction <$ string "/----/"
    <|> (U.f1ts >>> HT.Unparsed)
        <$> between (string PM._unparsedFnStart) (string PM._unparsedFnEnd) (many1 $ noneOf PM._unparsedFnTerminals)


numberJsExpr :: Parser String HT.JsExpr
numberJsExpr = do
  _ <- U.spaces
  n <- number
  _ <- U.spaces
  pure $ HT.Val $ HT.Number n


piJsExpr :: Parser String HT.JsExpr
piJsExpr = do
  _ <- U.spaces
  _ <- string "Math.PI"
  _ <- U.spaces
  pure $ HT.Val HT.Pi


mouseXJsExpr :: Parser String HT.JsExpr
mouseXJsExpr = do
  U.betweenSpaces $ string "mouse.x" *> pure (HT.Val HT.MouseX)


mouseYJsExpr :: Parser String HT.JsExpr
mouseYJsExpr = do
  U.betweenSpaces $ string "mouse.y" *> pure (HT.Val HT.MouseY)


fftJsExpr :: Parser String HT.JsExpr
fftJsExpr = do
  _ <- U.spaces
  _ <- string "a.fft["
  _ <- U.spaces
  i <- intDecimal
  _ <- U.spaces
  _ <- string "]"
  _ <- U.spaces
  pure $ HT.Val $ HT.Fft $ HT.AudioBin i


mathJsExpr :: Parser String HT.JsExpr
mathJsExpr = do
  _ <- U.spaces
  _ <- string "Math."
  method <- many1 U.tokenChar
  _ <- U.spaces
  _ <- string "("
  _ <- U.spaces
  mbJsExpr <- optionMaybe inlineExprParser
  _ <- U.spaces
  _ <- string ")"
  _ <- U.spaces
  pure $ HT.Math (U.f1ts method) mbJsExpr



widthJsExpr :: Parser String HT.JsExpr
widthJsExpr = do
  _ <- U.spaces
  _ <- string "width"
  _ <- U.spaces
  pure $ HT.Val HT.Width


heightJsExpr :: Parser String HT.JsExpr
heightJsExpr = do
  _ <- U.spaces
  _ <- string "height"
  _ <- U.spaces
  pure $ HT.Val HT.Height


timeJsExpr :: Parser String HT.JsExpr
timeJsExpr = do
  _ <- U.spaces
  _ <- string "time"
  _ <- U.spaces
  pure $ HT.Val HT.Time


bracketsJsExpr :: Parser String HT.JsExpr
bracketsJsExpr = do
  _ <- U.spaces
  _ <- string "("
  _ <- U.spaces
  jsexpr <- inlineExprParser
  _ <- U.spaces
  _ <- string ")"
  _ <- U.spaces
  pure $ HT.Brackets jsexpr


operand :: Parser String HT.JsExpr
operand =
  try numberJsExpr
  <|> try piJsExpr
  <|> try timeJsExpr
  <|> try widthJsExpr
  <|> try heightJsExpr
  <|> try fftJsExpr
  <|> try mouseXJsExpr
  <|> try mouseYJsExpr
  -- FIXME: valuesJsExpr
  <|> try (defer \_ -> mathJsExpr)
  <|> try (defer \_ -> bracketsJsExpr)


inlineExprParser :: Parser String HT.JsExpr
inlineExprParser =
  buildExprParser [ [ Infix (string "/" $> HT.DivE) AssocRight ]
                  , [ Infix (string "*" $> HT.MulE) AssocRight ]
                  , [ Infix (string "-" $> HT.SubE) AssocRight ]
                  , [ Infix (string "+" $> HT.AddE) AssocRight ]
                  , [ Infix (string "%" $> HT.ModE) AssocRight ]
                  ] $ defer (\_ -> operand)


{-
instance HasParser_ WrapRepr where
    parser = wrap


instance HasParser HT.Value where
    parser = value


instance HasParser HT.AudioBin where
    parser = audioBin


instance HasParser HT.Texture where
    parser = texture


instance HasParser HT.Source where
    parser = source


instance HasParser HT.OutputN where
    parser = outputN
-}


decodeImpl :: forall x. HasParser x => String -> Maybe x
decodeImpl s =
    case runParser s parser of
        Left _ -> Nothing
        Right result -> Just result


parseArgs :: forall arg. Parser String arg -> Int -> Parser String (Array arg)
parseArgs argp n =
    replicateA n (argp <* string PM._argSep)


parseNamedArg :: forall arg. Parser String arg -> Parser String (String /\ arg)
parseNamedArg parg = do
    name <- many1 alphaNum
    _ <- string "::"
    v <- parg
    pure $ U.f1ts name /\ v


parseNamedArgs :: forall arg. Parser String arg -> Int -> Parser String (Array (String /\ arg))
parseNamedArgs =
    parseArgs <<< parseNamedArg


parseArgsHelper :: forall arg x. Parser String arg -> Int -> (Array arg -> Maybe x) -> Parser String x
parseArgsHelper parg n f =
    parseArgs parg n >>= \arr ->
        case f arr of
            Just res -> pure res
            Nothing -> fail $ "Required " <> show n <> " arguments but got " <> show (Array.length arr)


parseArgsHelperV :: forall x. Int -> (Array HT.Value -> Maybe x) -> Parser String x
parseArgsHelperV =
    parseArgsHelper value


parseNamedArgsHelper :: forall x arg. Parser String arg -> Int -> (Array (String /\ arg) -> Maybe x) -> Parser String x
parseNamedArgsHelper =
    parseArgsHelper <<< parseNamedArg


parseNamedArgsHelperTOrV :: forall x. Int -> (Array (String /\ HT.TOrV) -> Maybe x) -> Parser String x
parseNamedArgsHelperTOrV =
    parseNamedArgsHelper tOrV


noArgs :: forall x. x -> Parser String x
noArgs x =
    string PM._argsEnd *> pure x
   --  parseArgsHelper 0 $ const $ Just x

args1 :: forall arg x. Parser String arg -> (arg -> x) -> Parser String x
args1 p f =
    parseArgsHelper p 1 $ \arr -> f <$> arr !! 0


parseArgs1V :: forall x. (HT.Value -> x) -> Parser String x
parseArgs1V f =
    parseArgsHelperV 1 $ \arr -> f <$> arr !! 0


parseArgs2V :: forall x. (HT.Value -> HT.Value -> x) -> Parser String x
parseArgs2V f =
    parseArgsHelperV 2 $ \arr -> f <$> arr !! 0 <*> arr !! 1


parseArgs3V :: forall x. (HT.Value -> HT.Value -> HT.Value -> x) -> Parser String x
parseArgs3V f =
    parseArgsHelperV 3 $ \arr -> f <$> arr !! 0 <*> arr !! 1 <*> arr !! 2


parseArgs4V :: forall x. (HT.Value -> HT.Value -> HT.Value -> HT.Value -> x) -> Parser String x
parseArgs4V f =
    parseArgsHelperV 4 $ \arr -> f <$> arr !! 0 <*> arr !! 1 <*> arr !! 2 <*> arr !! 3


parseArgs5V :: forall x. (HT.Value -> HT.Value -> HT.Value -> HT.Value -> HT.Value -> x) -> Parser String x
parseArgs5V f =
    parseArgsHelperV 4 $ \arr -> f <$> arr !! 0 <*> arr !! 1 <*> arr !! 2 <*> arr !! 3  <*> arr !! 4



findFnCode :: String -> Maybe HT.DepFn
findFnCode str =
    case runParser str inlineExprParser of
        Left _ -> Just $ HT.Unparsed str
        Right jsExpr -> Just $ HT.UserExpr jsExpr


findValues :: String -> Maybe HT.Values
findValues = const $ Just $ HT.Values [] -- FIXME