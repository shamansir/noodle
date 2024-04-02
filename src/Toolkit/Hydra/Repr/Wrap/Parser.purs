module Toolkit.Hydra.Repr.Wrap.Parser where
-- TODO: may be move value parsers to Hydra.Lang, but these parsers here encode NDF file structure, they are different from SketchParser,
-- but JsExpr parser could be separated since it is the same everywhere

import Data.FromToFile

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
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Parsing (Parser, runParser, fail)
import Parsing.String (string)
import Parsing.Combinators (try, many1, sepBy1, between, sepBy, replicateA, many1Till, optional, optionMaybe)
import Parsing.String.Basic (space, number, intDecimal, alphaNum, takeWhile1, noneOf)
import Parsing.Expr (buildExprParser, Assoc(..), Operator(..))


import Toolkit.Hydra.Repr.Wrap
import Toolkit.Hydra.Types as T
import Toolkit.Hydra.Lang.SketchParser.Utils as U
import Toolkit.Hydra.Lang.Fn (Fn, fnOf) as Lang
import Toolkit.Hydra.Lang.Fn (empty, argsCount, out1) as Fn


-- newtype HasParser = HasParser WrapRepr

class HasParser x where
    parser :: Parser String x


{- Orphan
instance HasParser x => Decode x where
    decode = decodeImpl
-}


wrap :: Parser String WrapRepr
wrap =
    foldMarkers
        [ marker $ "U" /\ Unit /\ (string "U" <#> const unit)
        , marker $ "TT" /\ TOrV <<< T.T /\ texture
        , marker $ "VV" /\ TOrV <<< T.V /\ value
        , marker $ "T" /\ Texture /\ texture
        , marker $ "V" /\ Value /\ value
        , marker $ "FN" /\ Fn /\ fn
        , marker $ "GLSL" /\ GlslFn /\ glsl
        ]


value :: Parser String T.Value
value =
    foldMarkers
        [ marker $ "N" /\ T.Number /\ number
        , marker $ "0" /\ const T.None /\ string "V"
        , marker $ "U" /\ const T.Undefined /\ string "V"
        , marker $ "T" /\ const T.Time /\ string "V"
        , marker $ "MX" /\ const T.MouseX /\ string "V"
        , marker $ "MY" /\ const T.MouseY /\ string "V"
        , marker $ "W" /\ const T.Width /\ string "V"
        , marker $ "H" /\ const T.Height /\ string "V"
        , marker $ "PI" /\ const T.Pi /\ string "V"
        , marker $ "A" /\ T.Fft /\ audioBin
        , marker $ "VA" /\ uncurry T.VArray /\ ((/\) <$> (defer \_ -> values) <*> (string " $$ " *> defer \_ -> ease))
        , marker $ "D" /\ T.Dep /\ fn
        ]


audioBin :: Parser String T.AudioBin
audioBin = T.AudioBin <$> (string "@" *> intDecimal)


values :: Parser String T.Values
values =
    T.Values
        <$> Array.fromFoldable
        <$>
        (between (string "%% ") (string " %%")
            $ flip sepBy (string " <> ") $ defer $ \f -> value
        )


outputN :: Parser String T.OutputN
outputN =
    string "O" *> intDecimal <#> case _ of
        0 -> T.Output0
        1 -> T.Output1
        2 -> T.Output2
        3 -> T.Output3
        4 -> T.Output4
        _ -> T.Output0


sourceN :: Parser String T.SourceN
sourceN =
    string "S" *> intDecimal <#> const T.Source0


texture :: Parser String T.Texture
texture =
    foldMarkers
        [ marker $ "EMP" /\ const T.Empty /\ string "T"
        , marker $ "S" /\ T.Start /\ source
        , marker $ "F" /\ uncurry T.Filter /\ do
            cop <- colorOp
            _ <- string T.texSep
            tex <- defer \_ -> texture
            _ <- string T.texsEnd
            pure $ tex /\ cop
        , marker $ "M" /\ uncurry T.ModulateWith /\ do
            what <- defer \_ -> texture
            _ <- string T.texSep
            with <- defer \_ -> texture
            _ <- string T.texSep
            mod <- modulate
            _ <- string T.texsEnd
            pure $ { what, with } /\ mod
        , marker $ "B" /\ uncurry T.BlendOf /\ do
            what <- defer \_ -> texture
            _ <- string T.texSep
            with <- defer \_ -> texture
            _ <- string T.texSep
            bl <- blend
            _ <- string T.texsEnd
            pure $ { what, with } /\ bl
        , marker $ "G" /\ uncurry T.Geometry /\ do
            tex <- defer \_ -> texture
            _ <- string T.texSep
            geo <- geometry
            _ <- string T.texsEnd
            pure $ tex /\ geo
        , marker $ "CALL" /\ uncurry T.CallGlslFn /\ do
            over <- defer \_ -> texture
            _ <- string T.texSep
            mbWith <- optionMaybe $ try $ do
                with <- defer \_ -> texture
                _ <- string T.texSep
                pure with
            fn <- langFn tOrV
            -- _ <- if Fn.argsCount fn > 0 then string T.texsEnd else
            -- _ <- optional space
            -- _ <- string T.argsEnd
            -- _ <- string T.texsEnd
            _ <- optional $ try $ string T.texsEnd
            pure $ { over, mbWith } /\ T.GlslFnRef fn
        ]


source :: Parser String T.Source
source =
    foldMarkers
        [ marker $ "S" /\ T.Solid /\ parseArgs4V \r g b a -> { r, g, b, a }
        , marker $ "G" /\ T.Gradient /\ parseArgs1V \speed -> { speed }
        , marker $ "N" /\ T.Noise /\ parseArgs2V \scale offset -> { scale, offset }
        , marker $ "OSC" /\ T.Osc /\ parseArgs3V \frequency sync offset -> { frequency, sync, offset }
        , marker $ "SCP" /\ T.Shape /\ parseArgs3V \sides radius smoothing -> { sides, radius, smoothing }
        , marker $ "V" /\ T.Voronoi /\ parseArgs3V \scale speed blending -> { scale, speed, blending }
        , marker $ "O" /\ T.Load /\ outputN
        , marker $ "X" /\ uncurry T.External /\ do
            src <- sourceN
            _ <- string T.argSep
            def <- extSource
            _ <- string T.argsEnd
            pure $ src /\ def

        ]


extSource :: Parser String T.ExtSource
extSource =
    foldMarkers
        [ marker $ "C" /\ T.Camera /\ intDecimal
        , marker $ "SK" /\ T.Sketch /\ (U.f1ts <$> many1 alphaNum)
        , marker $ "V" /\ const T.Video /\ string "X"
        , marker $ "U" /\ const T.Unclear /\ string "X"
        ]


ease :: Parser String T.Ease
ease = --pure T.Linear
    foldMarkers
        [ marker $ "LIN" /\ const T.Linear /\ string "E"
        , marker $ "FST" /\ T.Fast /\ defer \_ -> value
        , marker $ "SMT" /\ T.Smooth /\ defer \_ -> value
        , marker $ "OFF" /\ T.Offset /\ defer \_ -> value
        , marker $ "IOC" /\ const T.InOutCubic /\ string "E"
        , marker $ "FIT" /\ fit /\ do
                                low <- value
                                _ <- string " < "
                                high <- value
                                pure $ low /\ high

        ]
    where
        fit (low /\ high) = T.Fit { low, high }


blend :: Parser String T.Blend
blend =
    foldMarkers
        [ marker $ "BLEND" /\ T.Blend /\ parseArgs1V identity
        , marker $ "ADD" /\ T.Add /\ parseArgs1V identity
        , marker $ "LAYER" /\ T.Layer /\ parseArgs1V identity
        , marker $ "MULT" /\ T.Mult /\ parseArgs1V identity
        , marker $ "SUB" /\ T.Sub /\ parseArgs1V identity
        , marker $ "DIFF" /\ const T.Diff /\ noArgs unit
        , marker $ "MASK" /\ const T.Mask /\ noArgs unit
        ]


colorOp :: Parser String T.ColorOp
colorOp =
    foldMarkers
        [ marker $ "R" /\ T.R /\ parseArgs2V \scale offset -> { scale, offset }
        , marker $ "G" /\ T.G /\ parseArgs2V \scale offset -> { scale, offset }
        , marker $ "B" /\ T.B /\ parseArgs2V \scale offset -> { scale, offset }
        , marker $ "A" /\ T.A /\ parseArgs2V \scale offset -> { scale, offset }
        , marker $ "POSTERIZE" /\ T.Posterize /\ parseArgs2V \bins gamma -> { bins, gamma }
        , marker $ "SHIFT" /\ T.Shift /\ parseArgs4V \r g b a -> { r, g, b, a }
        , marker $ "COLOR" /\ T.Color /\ parseArgs4V \r g b a -> { r, g, b, a }
        , marker $ "LUMA" /\ T.Luma /\ parseArgs2V \threshold tolerance -> { threshold, tolerance }
        , marker $ "TRESH" /\ T.Thresh /\ parseArgs2V \threshold tolerance -> { threshold, tolerance }
        , marker $ "INVERT" /\ T.Invert /\ defer \_ -> value
        , marker $ "CONTRAST" /\ T.Contrast /\ defer \_ -> value
        , marker $ "BRIGHTNESS" /\ T.Brightness /\ defer \_ -> value
        , marker $ "SATURATE" /\ T.Saturate /\ defer \_ -> value
        , marker $ "HUE" /\ T.Brightness /\ defer \_ -> value
        , marker $ "COLORAMA" /\ T.Saturate /\ defer \_ -> value
        ]


modulate :: Parser String T.Modulate
modulate =
    foldMarkers
        [ marker $ "MODHUE" /\ T.ModHue /\ defer \_ -> value
        , marker $ "MODULATE" /\ T.Modulate /\ defer \_ -> value
        , marker $ "MODKALEID" /\ T.ModKaleid /\ parseArgs1V \nSides -> { nSides }
        , marker $ "MODPIXELATE" /\ T.ModPixelate /\ parseArgs2V \multiple offset -> { multiple, offset }
        , marker $ "MODREPEATX" /\ T.ModRepeatX /\ parseArgs2V \reps offset -> { reps, offset }
        , marker $ "MODREPEATY" /\ T.ModRepeatY /\ parseArgs2V \reps offset -> { reps, offset }
        , marker $ "MODREPEAT" /\ T.ModRepeat /\ parseArgs4V \repeatX repeatY offsetX offsetY -> { repeatX, repeatY, offsetX, offsetY }
        , marker $ "MODROTATE" /\ T.ModRotate /\ parseArgs2V \multiple offset ->  { multiple, offset }
        , marker $ "MODSCALE" /\ T.ModScale /\ parseArgs2V \multiple offset ->  { multiple, offset }
        , marker $ "MODSCROLLX" /\ T.ModScrollX /\ parseArgs2V \scrollX speed -> { scrollX, speed }
        , marker $ "MODSCROLLY" /\ T.ModScrollY /\ parseArgs2V \scrollY speed -> { scrollY, speed }
        , marker $ "MODSCROLL" /\ T.ModScroll /\ parseArgs4V \scrollX scrollY speedX speedY -> { scrollX, scrollY, speedX, speedY }
        ]


geometry :: Parser String T.Geometry
geometry =
    foldMarkers
        [ marker $ "KALEID" /\ T.GKaleid /\ parseArgs1V \nSides -> { nSides }
        , marker $ "PIXELATE" /\ T.GPixelate /\ parseArgs2V \pixelX pixelY -> { pixelX, pixelY }
        , marker $ "REPEATX" /\ T.GRepeatX /\ parseArgs2V \reps offset -> { reps, offset }
        , marker $ "REPEATY" /\ T.GRepeatY /\ parseArgs2V \reps offset -> { reps, offset }
        , marker $ "REPEAT" /\ T.GRepeat /\ parseArgs4V \repeatX repeatY offsetX offsetY -> { repeatX, repeatY, offsetX, offsetY }
        , marker $ "ROTATE" /\ T.GRotate /\ parseArgs2V \angle speed ->  { angle, speed }
        , marker $ "SCALE" /\ T.GScale /\ parseArgs5V \amount xMult yMult offsetX offsetY ->  { amount, xMult, yMult, offsetX, offsetY }
        , marker $ "SCROLLX" /\ T.GScrollX /\ parseArgs2V \scrollX speed -> { scrollX, speed }
        , marker $ "SCROLLY" /\ T.GScrollY /\ parseArgs2V \scrollY speed -> { scrollY, speed }
        , marker $ "SCROLL" /\ T.GScroll /\ parseArgs4V \scrollX scrollY speedX speedY -> { scrollX, scrollY, speedX, speedY }
        ]


glslKind :: Parser String T.GlslFnKind
glslKind =
        string "SRC" $> T.FnSrc
    <|> string "CRD" $> T.FnCoord
    <|> string "CCR" $> T.FnCombineCoord
    <|> string "CMB" $> T.FnCombine
    <|> string "CLR" $> T.FnColor


glsl :: Parser String T.GlslFn
glsl = do
    kind <- glslKind
    _ <- space
    code <- between (string T.glslStart) (string T.glslEnd) (many1 $ noneOf T.glslTerminals)
    _ <- space
    fd <- langFn tOrV
    pure $ T.GlslFn $ kind /\ T.GlslFnCode (U.f1ts code) /\ fd


langFn ::forall x. Parser String x -> Parser String (Lang.Fn x Unit)
langFn argParser = do
    name <- many1 alphaNum
    _ <- space
    argsN <- intDecimal
    _ <- space
    args <- parseNamedArgsHelper argParser argsN (identity >>> Just)
    -- args <- if (argsN > 0) then
    --             space *> parseNamedArgsHelper argParser argsN (identity >>> Just)
    --         else pure []
    pure $ Lang.fnOf (U.f1ts name) args # Fn.out1 ("out" /\ unit)


tOrV :: Parser String T.TOrV
tOrV =
    foldMarkers
        [ marker $ "TT" /\ T.T /\ defer \_ -> texture
        , marker $ "VV" /\ T.V /\ value
        ]


fn :: Parser String T.Fn
fn =
    T.NoAction <$ string "/----/"
    <|> (U.f1ts >>> T.Unparsed)
        <$> between (string T.unparsedFnStart) (string T.unparsedFnEnd) (many1 $ noneOf T.unparsedFnTerminals)


numberJsExpr :: Parser String T.JsExpr
numberJsExpr = do
  _ <- U.spaces
  n <- number
  _ <- U.spaces
  pure $ T.Val $ T.Number n


piJsExpr :: Parser String T.JsExpr
piJsExpr = do
  _ <- U.spaces
  _ <- string "Math.PI"
  _ <- U.spaces
  pure $ T.Val T.Pi


mouseXJsExpr :: Parser String T.JsExpr
mouseXJsExpr = do
  U.betweenSpaces $ string "mouse.x" *> pure (T.Val T.MouseX)


mouseYJsExpr :: Parser String T.JsExpr
mouseYJsExpr = do
  U.betweenSpaces $ string "mouse.y" *> pure (T.Val T.MouseY)


fftJsExpr :: Parser String T.JsExpr
fftJsExpr = do
  _ <- U.spaces
  _ <- string "a.fft["
  _ <- U.spaces
  i <- intDecimal
  _ <- U.spaces
  _ <- string "]"
  _ <- U.spaces
  pure $ T.Val $ T.Fft $ T.AudioBin i


mathJsExpr :: Parser String T.JsExpr
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
  pure $ T.Math (U.f1ts method) mbJsExpr



widthJsExpr :: Parser String T.JsExpr
widthJsExpr = do
  _ <- U.spaces
  _ <- string "width"
  _ <- U.spaces
  pure $ T.Val T.Width


heightJsExpr :: Parser String T.JsExpr
heightJsExpr = do
  _ <- U.spaces
  _ <- string "height"
  _ <- U.spaces
  pure $ T.Val T.Height


timeJsExpr :: Parser String T.JsExpr
timeJsExpr = do
  _ <- U.spaces
  _ <- string "time"
  _ <- U.spaces
  pure $ T.Val T.Time


bracketsJsExpr :: Parser String T.JsExpr
bracketsJsExpr = do
  _ <- U.spaces
  _ <- string "("
  _ <- U.spaces
  jsexpr <- inlineExprParser
  _ <- U.spaces
  _ <- string ")"
  _ <- U.spaces
  pure $ T.Brackets jsexpr


operand :: Parser String T.JsExpr
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


inlineExprParser :: Parser String T.JsExpr
inlineExprParser =
  buildExprParser [ [ Infix (string "/" $> T.DivE) AssocRight ]
                  , [ Infix (string "*" $> T.MulE) AssocRight ]
                  , [ Infix (string "-" $> T.SubE) AssocRight ]
                  , [ Infix (string "+" $> T.AddE) AssocRight ]
                  , [ Infix (string "%" $> T.ModE) AssocRight ]
                  ] $ defer (\_ -> operand)


instance HasParser WrapRepr where
    parser = wrap


instance HasParser T.Value where
    parser = value


instance HasParser T.AudioBin where
    parser = audioBin


instance HasParser T.Texture where
    parser = texture


instance HasParser T.Source where
    parser = source


instance HasParser T.OutputN where
    parser = outputN


decodeImpl :: forall x. HasParser x => String -> Maybe x
decodeImpl s =
    case runParser s parser of
        Left _ -> Nothing
        Right result -> Just result


parseArgs :: forall arg. Parser String arg -> Int -> Parser String (Array arg)
parseArgs argp n =
    replicateA n (argp <* string T.argSep)


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


parseArgsHelperV :: forall x. Int -> (Array T.Value -> Maybe x) -> Parser String x
parseArgsHelperV =
    parseArgsHelper value


parseNamedArgsHelper :: forall x arg. Parser String arg -> Int -> (Array (String /\ arg) -> Maybe x) -> Parser String x
parseNamedArgsHelper =
    parseArgsHelper <<< parseNamedArg


parseNamedArgsHelperTOrV :: forall x. Int -> (Array (String /\ T.TOrV) -> Maybe x) -> Parser String x
parseNamedArgsHelperTOrV =
    parseNamedArgsHelper tOrV


noArgs :: forall x. x -> Parser String x
noArgs x =
    string T.argsEnd *> pure x
   --  parseArgsHelper 0 $ const $ Just x


parseArgs1V :: forall x. (T.Value -> x) -> Parser String x
parseArgs1V f =
    parseArgsHelperV 1 $ \arr -> f <$> arr !! 0


parseArgs2V :: forall x. (T.Value -> T.Value -> x) -> Parser String x
parseArgs2V f =
    parseArgsHelperV 2 $ \arr -> f <$> arr !! 0 <*> arr !! 1


parseArgs3V :: forall x. (T.Value -> T.Value -> T.Value -> x) -> Parser String x
parseArgs3V f =
    parseArgsHelperV 3 $ \arr -> f <$> arr !! 0 <*> arr !! 1 <*> arr !! 2


parseArgs4V :: forall x. (T.Value -> T.Value -> T.Value -> T.Value -> x) -> Parser String x
parseArgs4V f =
    parseArgsHelperV 4 $ \arr -> f <$> arr !! 0 <*> arr !! 1 <*> arr !! 2 <*> arr !! 3


parseArgs5V :: forall x. (T.Value -> T.Value -> T.Value -> T.Value -> T.Value -> x) -> Parser String x
parseArgs5V f =
    parseArgsHelperV 4 $ \arr -> f <$> arr !! 0 <*> arr !! 1 <*> arr !! 2 <*> arr !! 3  <*> arr !! 4



findFnCode :: String -> Maybe T.Fn
findFnCode str =
    case runParser str inlineExprParser of
        Left _ -> Just $ T.Unparsed str
        Right jsExpr -> Just $ T.UserExpr jsExpr


findValues :: String -> Maybe T.Values
findValues = const $ Just $ T.Values [] -- FIXME
