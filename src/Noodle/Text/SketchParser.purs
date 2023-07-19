module Noodle.Text.SketchParser where

import Prelude

import Data.Foldable (class Foldable, fold)
import Data.Semigroup.Foldable (class Foldable1)
import Effect (Effect)
import Parsing (Parser, runParser)
import Parsing.String (char, string, anyChar, anyTill)
import Parsing.String.Basic (alphaNum, digit, space, number, intDecimal)
import Parsing.Combinators (try, many1Till, sepEndBy, sepEndBy1, many1)
import Control.Alt ((<|>))
import Data.Array (many, fromFoldable)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.String as String
import Data.String.NonEmpty.CodeUnits as CU
import Data.String.NonEmpty.Internal as StringX
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))

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

data Expr
    = Token String
    | Num (Array Char) (Array Char)
    | ChainStart String (Array Expr)
    | ChainContinue String (Array Expr)
    | FnInline String
    | Comment String
    | EmptyLine


derive instance Eq Expr


data Script = Script (Array Expr)


tokenChar :: Parser String Char
tokenChar = alphaNum


--nelToString p = p <#> CU.fromFoldable1 <#> StringX.toString


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
    idigits <- many digit
    _ <- string "."
    fldigits <- many1 digit
    pure $ Num (fromFoldable idigits) (fromFoldable fldigits)


chainStart :: Parser String Expr
chainStart = do
    -- TODO
    pure $ ChainStart "foo" []


chainContinue :: Parser String Expr
chainContinue = do
    -- TODO
    pure $ ChainContinue "foo" []


fnInline :: Parser String Expr
fnInline = do
    _ <- string "()"
    -- TODO
    pure $ FnInline "foo"


emptyLine :: Parser String Expr
emptyLine = do
    _ <- many space
    eol
    pure $ EmptyLine


line :: Parser String Expr
line =
  try comment
  <|> try chainStart
  <|> try chainContinue
  <|> try emptyLine


parser :: Parser String Script
parser = do
  lines <- many1 line
  pure $ Script $ fromFoldable lines


instance Show Expr where
  show = case _ of
    Token str -> "%" <> str <> "%"
    Num is fs -> show is <> "." <> show fs
    ChainStart fn exprs ->
      fn <> " " <> String.joinWith " @ " (show <$> exprs)
    ChainContinue fn exprs ->
      "." <> fn <> " " <> String.joinWith " @ " (show <$> exprs)
    FnInline str ->
      ">> " <> str <> " <<"
    Comment str ->
      "// " <> str <> " //"
    EmptyLine ->
      "***"


instance Show Script where
  show :: Script -> String
  show (Script exprs) =
    String.joinWith "\n" $ show <$> exprs