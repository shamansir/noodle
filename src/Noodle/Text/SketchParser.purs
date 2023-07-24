module Main where

import Prelude

-- import TryPureScript (h1, h3, p, text, render, code)
import Data.Foldable (class Foldable, fold)
import Data.Semigroup.Foldable (class Foldable1)
import Data.CodePoint.Unicode as U
import Data.String.CodePoints (codePointFromChar)
import Effect (Effect)
import Parsing (Parser, runParser)
import Parsing.String (char, string, anyChar, anyTill, satisfy)
import Parsing.String.Basic (alphaNum, digit, space, number, intDecimal)
import Parsing.Combinators (optional, option, between, try, many1Till, sepBy, sepEndBy, sepEndBy1, many1, manyTill_)
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
    | FnInline String
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
    token <- many1 tokenChar
    exprs <- between beforeArgs afterArgs $ sepBy expr separator
    chainCont <- option [] $ many $ do
      _ <- many spaceOrEol
      _ <- char '.'
      token <- many1 tokenChar
      innerExprs <- between beforeArgs afterArgs $ sepBy expr separator
      _ <- many spaceOrEol
      pure $ (f1ts token) /\ fromFoldable innerExprs
    -- _ <- optional $ try eol
    pure $ Chain (f1ts token) (fromFoldable exprs) (fromFoldable chainCont)


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
    , h3 $ code $ text $ show $ runParser test2 script
    , h3 $ code $ text $ show $ runParser test3 script
    -- , h3 $ code $ text $ show $ runParser "()=>Math.sin(time)+1*3" fnInline
    -- , h3 $ code $ text $ show $ runParser "()=>a.fft[1]*2" fnInline
    -- , h3 $ code $ text $ show $ runParser "shape(()=>Math.sin(time)+1*3)" script
    -- , h3 $ code $ text $ show $ runParser "shape(()=>Math.sin(time)+1*3, .5,.01)" chainStart
    -- , h3 $ code $ text $ show $ runParser "shape()" chainStart
    -- , h3 $ code $ text $ show $ runParser "src(o1)" chainStart
    -- , h3 $ code $ text $ show $ runParser "shape(0.5)" chainStart
    -- , h3 $ code $ text $ show $ runParser "shape(0.5,.2)" chainStart
    -- , h3 $ code $ text $ show $ runParser "shape(0.5,.2, 0.2)" chainStart
    -- , h3 $ code $ text $ show $ runParser "shape(1.0, .5,.01)" chainStart
    -- , h3 $ code $ text $ show $ runParser "shape(5,3, o1, .5,.01)" chainStart
    -- , h3 $ code $ text $ show $ runParser ".repeat(5,3, ()=>a.fft[0]*2, ()=>a.fft[1]*2)" chainContinue
    -- , h3 $ code $ text $ show $ runParser ".repeat(5,3, ()=>a.fft[0]*2, ()=>a.fft[1]*2)" chainContinue
    ]
-}