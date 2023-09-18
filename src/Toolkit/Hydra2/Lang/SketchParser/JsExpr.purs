module Toolkit.Hydra2.Lang.SketchParser.JsExpr where

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


data JsExpr
  = INum Number
  | Pi
  | Time
  | Div JsExpr JsExpr
  | Mul JsExpr JsExpr
  | Sub JsExpr JsExpr
  | Add JsExpr JsExpr
  | Mod JsExpr JsExpr
  | Fft Int
  | Math String (Maybe JsExpr)
  | Brackets JsExpr
  | MouseX
  | MouseY
  | Width
  | Height


derive instance Eq JsExpr


numberJsExpr :: Parser String JsExpr
numberJsExpr = do
  _ <- spaces
  n <- number
  _ <- spaces
  pure $ INum n


piJsExpr :: Parser String JsExpr
piJsExpr = do
  _ <- spaces
  _ <- string "Math.PI"
  _ <- spaces
  pure $ Pi


mouseXJsExpr :: Parser String JsExpr
mouseXJsExpr = do
  betweenSpaces $ string "mouse.x" *> pure MouseX


mouseYJsExpr :: Parser String JsExpr
mouseYJsExpr = do
  betweenSpaces $ string "mouse.y" *> pure MouseY


fftJsExpr :: Parser String JsExpr
fftJsExpr = do
  _ <- spaces
  _ <- string "a.fft["
  _ <- spaces
  i <- intDecimal
  _ <- spaces
  _ <- string "]"
  _ <- spaces
  pure $ Fft i


mathJsExpr :: Parser String JsExpr
mathJsExpr = do
  _ <- spaces
  _ <- string "Math."
  method <- many1 tokenChar
  _ <- spaces
  _ <- string "("
  _ <- spaces
  mbJsExpr <- optionMaybe inlineExprParser
  _ <- spaces
  _ <- string ")"
  _ <- spaces
  pure $ Math (f1ts method) mbJsExpr



widthJsExpr :: Parser String JsExpr
widthJsExpr = do
  _ <- spaces
  _ <- string "width"
  _ <- spaces
  pure Width


heightJsExpr :: Parser String JsExpr
heightJsExpr = do
  _ <- spaces
  _ <- string "height"
  _ <- spaces
  pure Height


timeJsExpr :: Parser String JsExpr
timeJsExpr = do
  _ <- spaces
  _ <- string "time"
  _ <- spaces
  pure Time


bracketsJsExpr :: Parser String JsExpr
bracketsJsExpr = do
  _ <- spaces
  _ <- string "("
  _ <- spaces
  jsexpr <- inlineExprParser
  _ <- spaces
  _ <- string ")"
  _ <- spaces
  pure $ Brackets jsexpr


operand :: Parser String JsExpr
operand =
  try numberJsExpr
  <|> try piJsExpr
  <|> try timeJsExpr
  <|> try widthJsExpr
  <|> try heightJsExpr
  <|> try fftJsExpr
  <|> try mouseXJsExpr
  <|> try mouseYJsExpr
  <|> try (defer \_ -> mathJsExpr)
  <|> try (defer \_ -> bracketsJsExpr)


inlineExprParser :: Parser String JsExpr
inlineExprParser =
  buildExprParser [ [ Infix (string "/" $> Div) AssocRight ]
                  , [ Infix (string "*" $> Mul) AssocRight ]
                  , [ Infix (string "-" $> Sub) AssocRight ]
                  , [ Infix (string "+" $> Add) AssocRight ]
                  , [ Infix (string "%" $> Mod) AssocRight ]
                  ] $ defer (\_ -> operand)


instance Show JsExpr where
  show (INum n) = show n
  show Pi = "Pi"
  show Time = "Time"
  show (Div a b) = show a <> "/" <> show b
  show (Mul a b) = show a <> "*" <> show b
  show (Add a b) = show a <> "+" <> show b
  show (Sub a b) = show a <> "-" <> show b
  show (Mod a b) = show a <> "%" <> show b
  show (Fft n) = "a.fft[" <> show n <> "]"
  show (Math method expr) = "Math." <> method <> "(" <> show expr <> ")"
  show (Brackets expr) = "(" <> show expr <> ")"
  show MouseX = "mouse.x"
  show MouseY = "mouse.y"
  show Width = "width"
  show Height = "height"


instance ToCode PS JsExpr where
  toCode :: Proxy PS -> JsExpr -> String
  toCode _ = case _ of
    INum n -> if n >= 0.0 then show n else "(" <> show n <> ")"
    Pi -> "pi"
    Time -> "ctx.time"
    Div a b -> "(" <> toCode pureScript a <> " / " <> toCode pureScript b <> ")"
    Mul a b -> "(" <> toCode pureScript a <> " * " <> toCode pureScript b <> ")"
    Add a b -> "(" <> toCode pureScript a <> " + " <> toCode pureScript b <> ")"
    Sub a b -> "(" <> toCode pureScript a <> " - " <> toCode pureScript b <> ")"
    Mod a b -> "(" <> toCode pureScript a <> " % " <> toCode pureScript b <> ")"
    Fft n -> "(a # fft h" <> show n <> ")"
    Brackets expr -> "(" <> toCode javaScript expr <> ")"
    MouseX -> "ctx.mouseX"
    MouseY -> "ctx.mouseY"
    Width -> "ctx.width"
    Height -> "ctx.height"
    Math method (Just expr) -> "(" <> method <> " $ " <> toCode pureScript expr <> ")"
    Math method Nothing -> method


instance ToCode JS JsExpr where
  toCode :: Proxy JS -> JsExpr -> String
  toCode _ = case _ of
    INum n -> show n
    Pi -> "Math.pi"
    Time -> "time"
    Div a b -> toCode javaScript a <> "/" <> toCode javaScript b
    Mul a b -> toCode javaScript a <> "*" <> toCode javaScript b
    Add a b -> toCode javaScript a <> "+" <> toCode javaScript b
    Sub a b -> toCode javaScript a <> "-" <> toCode javaScript b
    Mod a b -> toCode javaScript a <> "%" <> toCode javaScript b
    Fft n -> "a.fft[" <> show n <> "]"
    Brackets expr -> "(" <> toCode javaScript expr <> ")"
    MouseX -> "mouse.x"
    MouseY -> "mouse.y"
    Width -> "width"
    Height -> "height"
    Math method (Just expr) -> "Math." <> method <> "(" <> toCode javaScript expr <> ")"
    Math method Nothing -> "Math." <> method <> "()"
