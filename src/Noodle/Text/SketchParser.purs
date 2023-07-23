module Main where

import Prelude

-- import TryPureScript (h1, h3, p, text, render, code)
import Data.Foldable (class Foldable, fold)
import Data.Semigroup.Foldable (class Foldable1)
import Effect (Effect)
import Parsing (Parser, runParser)
import Parsing.String (char, string, anyChar, anyTill, satisfy)
import Parsing.String.Basic (alphaNum, digit, space, number, intDecimal)
import Parsing.Combinators (between, try, many1Till, sepBy, sepEndBy, sepEndBy1, many1, manyTill_)
import Control.Alt ((<|>))
import Control.Lazy (defer)

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
    _ <- many space
    idigits <- many digit
    _ <- char '.'
    fldigits <- many1 digit
    pure $ Num (fromFoldable idigits) (fromFoldable fldigits)



chainStart :: Parser String Expr
chainStart = do
    _ <- many space
    token <- many1 tokenChar
    exprs <- between (char '(') (char ')') $ sepBy expr $ char ','
    pure $ ChainStart (f1ts token) $ fromFoldable exprs


chainContinue :: Parser String Expr
chainContinue = do
    _ <- many space
    _ <- char '.'
    token <- many1 tokenChar
    exprs <- between (char '(') (char ')') $ sepBy expr $ char ','
    pure $ ChainContinue "foo" $ fromFoldable exprs


fnInline :: Parser String Expr
fnInline = do
    _ <- string "()=>"
    -- inner <- many1Till anyChar (try $ char ',')
    -- inner <- many1Till anyChar (try $ char ',')
    inner <- many1 $ satisfy ((/=) ',')
    -- TODO
    pure $ FnInline $ f1ts inner


emptyLine :: Parser String Expr
emptyLine = do
    _ <- many space
    eol
    pure $ EmptyLine


expr :: Parser String Expr
expr =
  try comment
  <|> try fnInline
  <|> try numberx
  <|> try (defer \_ -> chainStart)
  <|> try (defer \_ -> chainContinue)


script :: Parser String Script
script = do
  lines <- many1 expr
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


{-
main :: Effect Unit
main =
  render $ fold
    [ h1 $ text "Test parsing script"

    , h3 $ code $ text $ show $ runParser myFile script
    , h3 $ code $ text $ show $ runParser "()=>Math.sin(time)+1*3" fnInline
    -- , h3 $ code $ text $ show $ runParser "shape(()=>Math.sin(time)+1*3)" script
    , h3 $ code $ text $ show $ runParser "shape(()=>Math.sin(time)+1*3, .5,.01)" chainStart
    , h3 $ code $ text $ show $ runParser "shape()" chainStart
    , h3 $ code $ text $ show $ runParser "shape(0.5)" chainStart
    , h3 $ code $ text $ show $ runParser "shape(0.5,.2)" chainStart
    , h3 $ code $ text $ show $ runParser "shape(0.5,.2, 0.2)" chainStart
    , h3 $ code $ text $ show $ runParser "shape(1.0, .5,.01)" chainStart
    ]
-}