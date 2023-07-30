module Toolkit.Hydra2.Lang.SketchParser.IExpr where

import Prelude


import Type.Proxy (Proxy)

import Data.Maybe (Maybe(..))

import Parsing (Parser)
import Parsing.String (string)
import Parsing.String.Basic (number, intDecimal)
import Parsing.Combinators (many1, optionMaybe, try)
import Parsing.Expr (buildExprParser, Assoc(..), Operator(..))

import Control.Alt ((<|>))
import Control.Lazy (defer)

import Toolkit.Hydra2.Lang.ToCode (class ToCode, NDF, PS, JS, pureScript, toCode, javaScript)


import Toolkit.Hydra2.Lang.SketchParser.Utils (betweenSpaces, f1ts, spaces, tokenChar)


data IExpr
  = INum Number
  | Pi
  | Time
  | Div IExpr IExpr
  | Mul IExpr IExpr
  | Sub IExpr IExpr
  | Add IExpr IExpr
  | Fft Int
  | Math String (Maybe IExpr)
  | Brackets IExpr
  | MouseX
  | MouseY
  | Width
  | Height


derive instance Eq IExpr


numberIExpr :: Parser String IExpr
numberIExpr = do
  _ <- spaces
  n <- number
  _ <- spaces
  pure $ INum n


piIExpr :: Parser String IExpr
piIExpr = do
  _ <- spaces
  _ <- string "Math.PI"
  _ <- spaces
  pure $ Pi


mouseXIExpr :: Parser String IExpr
mouseXIExpr = do
  betweenSpaces $ string "mouse.x" *> pure MouseX


mouseYIExpr :: Parser String IExpr
mouseYIExpr = do
  betweenSpaces $ string "mouse.y" *> pure MouseY


fftIExpr :: Parser String IExpr
fftIExpr = do
  _ <- spaces
  _ <- string "a.fft["
  _ <- spaces
  i <- intDecimal
  _ <- spaces
  _ <- string "]"
  _ <- spaces
  pure $ Fft i


mathIExpr :: Parser String IExpr
mathIExpr = do
  _ <- spaces
  _ <- string "Math."
  method <- many1 tokenChar
  _ <- spaces
  _ <- string "("
  _ <- spaces
  mbIExpr <- optionMaybe inlineExprParser
  _ <- spaces
  _ <- string ")"
  _ <- spaces
  pure $ Math (f1ts method) mbIExpr


timeIExpr :: Parser String IExpr
timeIExpr = do
  _ <- spaces
  _ <- string "time"
  _ <- spaces
  pure Time


bracketsIExpr :: Parser String IExpr
bracketsIExpr = do
  _ <- spaces
  _ <- string "("
  _ <- spaces
  iexpr <- inlineExprParser
  _ <- spaces
  _ <- string ")"
  _ <- spaces
  pure $ Brackets iexpr

operand :: Parser String IExpr
operand =
  try numberIExpr
  <|> try piIExpr
  <|> try timeIExpr
  <|> try fftIExpr
  <|> try mouseXIExpr
  <|> try mouseYIExpr
  <|> try (defer \_ -> mathIExpr)
  <|> try (defer \_ -> bracketsIExpr)


inlineExprParser :: Parser String IExpr
inlineExprParser =
  buildExprParser [ [ Infix (string "/" $> Div) AssocRight ]
                  , [ Infix (string "*" $> Mul) AssocRight ]
                  , [ Infix (string "-" $> Sub) AssocRight ]
                  , [ Infix (string "+" $> Add) AssocRight ]
                  ] $ defer (\_ -> operand)

instance Show IExpr where
  show (INum n) = show n
  show Pi = "Pi"
  show Time = "Time"
  show (Div a b) = show a <> "/" <> show b
  show (Mul a b) = show a <> "*" <> show b
  show (Add a b) = show a <> "+" <> show b
  show (Sub a b) = show a <> "-" <> show b
  show (Fft n) = "a.fft[" <> show n <> "]"
  show (Math method expr) = "Math." <> method <> "(" <> show expr <> ")"
  show (Brackets expr) = "(" <> show expr <> ")"
  show MouseX = "mouse.x"
  show MouseY = "mouse.y"
  show Width = "width"
  show Height = "height"


instance ToCode PS IExpr where
  toCode :: Proxy PS -> IExpr -> String
  toCode _ = case _ of
    INum n -> if n >= 0.0 then show n else "(" <> show n <> ")"
    Pi -> "pi"
    Time -> "ctx.time"
    Div a b -> "(" <> toCode pureScript a <> " / " <> toCode pureScript b <> ")"
    Mul a b -> "(" <> toCode pureScript a <> " * " <> toCode pureScript b <> ")"
    Add a b -> "(" <> toCode pureScript a <> " + " <> toCode pureScript b <> ")"
    Sub a b -> "(" <> toCode pureScript a <> " - " <> toCode pureScript b <> ")"
    Fft n -> "(a # fft h" <> show n <> ")"
    Brackets expr -> "(" <> show expr <> ")"
    MouseX -> "ctx.mouseX"
    MouseY -> "ctx.mouseY"
    Width -> "ctx.width"
    Height -> "ctx.height"
    Math method (Just expr) -> "(" <> method <> " $ " <> toCode pureScript expr <> ")"
    Math method Nothing -> method


instance ToCode JS IExpr where
  toCode :: Proxy JS -> IExpr -> String
  toCode _ = case _ of
    INum n -> show n
    Pi -> "Math.pi"
    Time -> "time"
    Div a b -> toCode javaScript a <> "/" <> toCode javaScript b
    Mul a b -> toCode javaScript a <> "*" <> toCode javaScript b
    Add a b -> toCode javaScript a <> "+" <> toCode javaScript b
    Sub a b -> toCode javaScript a <> "-" <> toCode javaScript b
    Fft n -> "a.fft[" <> show n <> "]"
    Brackets expr -> "(" <> show expr <> ")"
    MouseX -> "mouse.x"
    MouseY -> "mouse.y"
    Width -> "width"
    Height -> "height"
    Math method (Just expr) -> "Math." <> method <> "(" <> toCode javaScript expr <> ")"
    Math method Nothing -> "Math." <> method <> "()"
