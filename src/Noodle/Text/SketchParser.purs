module Noodle.Text.SketchParser where

import Prelude

import Type.Proxy (Proxy)

-- import TryPureScript (h1, h3, p, text, render, code)
import Data.Semigroup.Foldable (class Foldable1)
import Data.CodePoint.Unicode as U
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Data.Array as Array
import Data.Foldable (foldr)
import Data.String.Pattern
import Data.Tuple (uncurry)

import Parsing (Parser)
import Parsing.String (char, string, anyChar, anyTill, satisfy)
import Parsing.String.Basic (alphaNum, digit, space, number, intDecimal)
import Parsing.Combinators (optional, option, optionMaybe, between, try, many1Till, sepBy, sepEndBy, sepEndBy1, many1, manyTill_)

import Control.Alt ((<|>))
import Control.Lazy (defer)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile, appendTextFile)

import Data.Array (many, fromFoldable)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.String as String
import Data.String.NonEmpty.CodeUnits as CU
import Data.String.NonEmpty.Internal as StringX
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Toolkit.Hydra2.Lang.ToCode (class ToCode, NDF, PS, JS, pureScript, toCode, javaScript)


myFile :: String
myFile =
  """// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// by Mahalia H-R
// IG: mm_hr_

shape(()=>Math.sin(time)+1*3, .5,.01)
.repeat(5,3, ()=>a.fft[0]*2, ()=>a.fft[1]*2)
.scrollY(.5,0.1)
.layer(
  src(o1)
  .mask(o0)
  .luma(.01, .1)
  .invert(.2)
)
.modulate(o1,.02)
.out(o0)

osc(40, 0.09, 0.9)
.color(.9,0,5)
.modulate(osc(10).rotate(1, 0.5))
.rotate(1, 0.2)
.out(o1)

render(o0)
// https://hydra.ojack.xyz/?sketch_id=mahalia_3
"""


test2 =
  """
  osc(40, 0.09, 0.9)
  .color(.9,0,5)
  .modulate(osc(10).rotate(1, 0.5))
  .rotate(1, 0.2)
  .out(o1)
  """


test3 =
  """
  shape(()=>Math.sin(time)+1*3, .5,.01)
  .repeat(5,3, ()=>a.fft[0]*2, ()=>a.fft[1]*2)
  .scrollY(.5,0.1)
  .modulate(o1,.02)
  .out(o0)

  osc(40, 0.09, 0.9)
  .color(.9,0,5)
  .rotate(1, 0.2)
  .out(o1)

  render(o0)
  """

data Expr
    = Token String
    | Num Number
    | Chain String (Array Expr) (Array (String /\ Array Expr))
    | FnInline String String
    | Comment String
    | EmptyLine


derive instance Eq Expr


data Script = Script (Array Expr)


tokenChar :: Parser String Char
tokenChar = alphaNum


spaceOrEol :: Parser String Char
spaceOrEol = satisfy $ \c -> U.isSpace (codePointFromChar c) || (c == '\n') || (c == '\t')


f1ts :: forall f. Foldable1 f => f Char -> String
f1ts = CU.fromFoldable1 >>> StringX.toString


tokenTill :: forall a. Parser String a -> Parser String (NonEmptyList Char)
tokenTill stopAt =
  many1Till tokenChar stopAt


eol :: Parser String Unit
eol = char '\n' *> pure unit


comment :: Parser String Expr
comment = do
    _ <- many space
    _ <- string "//"
    _ <- many space
    commentText <- many1Till anyChar eol
    pure $ Comment $ f1ts commentText


numberx :: Parser String Expr
numberx = do
    num <- number
    pure $ Num num
  {- do
    idigits <- many digit
    _ <- optional $ char '.'
    fldigits <- many1 digit
    pure $ Num (fromFoldable idigits) (fromFoldable fldigits) -}


beforeArgs :: Parser String Unit
beforeArgs = do
  _ <- many spaceOrEol
  _ <- char '('
  _ <- many spaceOrEol
  pure unit


afterArgs :: Parser String Unit
afterArgs = do
  _ <- many spaceOrEol
  _ <- char ')'
  _ <- many spaceOrEol
  pure unit


separator :: Parser String Unit
separator = do
  _ <- many spaceOrEol
  _ <- char ','
  _ <- many spaceOrEol
  pure unit


chain :: Parser String Expr
chain = do
    _ <- many spaceOrEol
    maybeSubj <- optionMaybe $ try $ do
      token <- many1 tokenChar
      _ <- many space
      _ <- char '.'
      pure token
    token <- many1 tokenChar
    exprs <- between beforeArgs afterArgs $ sepBy expr separator
    chainCont <- option [] $ many $ do
      _ <- many spaceOrEol
      _ <- char '.'
      token <- many1 tokenChar
      innerExprs <- between beforeArgs afterArgs $ sepBy expr separator
      _ <- many spaceOrEol
      pure $ (f1ts token) /\ fromFoldable innerExprs
    _ <- optional $ try $ char ';'
    pure $ Chain (f1ts token) (fromFoldable exprs) (fromFoldable chainCont)


fnInline :: Parser String Expr
fnInline = do
    _ <- char '('
    args <- many tokenChar
    _ <- string ")=>"
    -- _ <- string "()=>"
    -- inner <- many1Till anyChar (try $ char ',')
    -- inner <- many1Till anyChar (try $ char ',')
    inner <- many1 $ satisfy $ \c -> c /= ',' && c /= ')'
    -- let charIdx =
    -- inner <- chainl1
    -- inner <- foldl ?wh 0 <$> many1 digit
    -- inner <- consumeWith ?wh
    -- TODO
    pure $ FnInline (fromCharArray args) $ f1ts inner


emptyLine :: Parser String Expr
emptyLine = do
    _ <- many space
    eol
    pure $ EmptyLine


token :: Parser String Expr
token = do
  token <- many1 tokenChar
  pure $ Token $ f1ts token


expr :: Parser String Expr
expr =
  try comment
  <|> try fnInline
  <|> try numberx
  <|> try (defer \_ -> chain)
  <|> try token


script :: Parser String Script
script = do
  lines <- many1 expr
  pure $ Script $ fromFoldable lines


traverseExprs :: (Expr -> Expr) -> Script -> Script
traverseExprs f (Script exprs) =
  Script $ deeper <$> exprs
  where
    deeper :: Expr -> Expr
    deeper (Chain name args tail) =
      f $ Chain name (deeper <$> args) $ map (map deeper) <$> tail
    deeper otherExpr = f otherExpr


replacements :: Array (Pattern /\ Replacement)
replacements = -- this helps to fix bracket parsing in `FnInline` without specifically parsing any contents of functions' code
  [ Pattern "sin(time)" /\ Replacement "sin{[time]}"
  ]


prepare :: String -> String
prepare str = foldr (uncurry String.replaceAll) str replacements


fixback :: Script -> Script
fixback = traverseExprs fixExpr
  where
    fixExpr (FnInline args contents) =
      FnInline args $ foldr (uncurry String.replaceAll) contents $ swapPR <$> replacements
    fixExpr otherExpr = otherExpr
    swapPR (Pattern pattern /\ Replacement replacement) =
      Pattern replacement /\ Replacement pattern


instance Show Expr where
  show = case _ of
    Token str -> "%" <> str <> "%"
    Num num -> show num
    Chain fn exprs cont ->
      fn <> " " <> String.joinWith " @ " (show <$> exprs) <> "-->" <>
      (String.joinWith " @@ " $
        (\(fnc /\ exprc) ->
          "." <> fnc <> " " <> String.joinWith " @ " (show <$> exprc)
        ) <$> cont
      )
    FnInline args str ->
      ">> " <> args <> " -> " <> str <> " <<"
    Comment str ->
      "// " <> str <> " //"
    EmptyLine ->
      "***"


instance Show Script where
  show :: Script -> String
  show (Script exprs) =
    String.joinWith "\n" $ show <$> exprs


instance ToCode PS Script where
  toCode :: Proxy PS -> Script -> String
  toCode _ (Script exprs) = String.joinWith "\n" $ toCode pureScript <$> exprs


instance ToCode PS Expr where
  toCode :: Proxy PS -> Expr -> String
  toCode _ = case _ of
    Token str -> str
    Num num -> "(n " <> show num <> ")"
    FnInline args code -> "(fn $ \\_ -> {- " <> friendlyArgs args <> " -> " <> code <> " -})"
    Chain fn args next ->
      ( if Array.length args == 1 then
        String.joinWith "" ((\arg -> toCode pureScript arg) <$> args) <> " # " <> fn
      else if Array.length args /= 0 then
          fn <> " " <> String.joinWith " " ((\arg -> toCode pureScript arg) <$> args)
      else
          fn
      )
      <>
      ( if Array.length next > 0 then
          "(\n" <> String.joinWith "\n" ((\(ifn /\ iargs) -> " # " <> (toCode pureScript $ Chain ifn iargs [])) <$> next) <> "\n)"
        else
          ""
      )
    Comment text -> "-- " <> text
    EmptyLine -> "\n\n"
    where
      friendlyArgs args = if String.null args then "_"  else args


instance ToCode JS Script where
  toCode :: Proxy JS -> Script -> String
  toCode _ (Script exprs) = String.joinWith "\n" $ toCode javaScript <$> exprs


instance ToCode JS Expr where
  toCode :: Proxy JS -> Expr -> String
  toCode _ = case _ of
    Token str -> str
    Num num -> show num
    FnInline args code -> "(" <> args <> ")=>" <> code
    Chain fn args next ->
      ( if Array.length args /= 0 then
          fn <> "(" <> String.joinWith "," ((\arg -> toCode javaScript arg) <$> args) <> ")"
        else
          fn <> "()"
      )
      <>
      ( if Array.length next > 0 then
          "\n" <> String.joinWith "\n" ((\(ifn /\ iargs) -> "." <> (toCode javaScript $ Chain ifn iargs [])) <$> next)
        else
          ""
      )
    Comment text -> "// " <> text
    EmptyLine -> "\n\n"
