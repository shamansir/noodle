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
import Data.Tuple (curry, uncurry)
import Data.Maybe (Maybe(..))

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


data Expr
    = Token String
    | Num Number
    | Chain
        { subj :: Maybe String
        , startOp :: String
        , args :: Array Expr
        , tail :: Array { op :: String, args :: Array Expr }
        }
    | FnInline { args :: String, code :: String }
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
    mbSubj <- optionMaybe $ try $ do
        subj <- many1 tokenChar
        _ <- many space
        _ <- char '.'
        pure subj
    startOp <- many1 tokenChar
    args <- between beforeArgs afterArgs $ sepBy expr separator
    tail <- option [] $ many $ do
        _ <- many spaceOrEol
        _ <- char '.'
        op <- many1 tokenChar
        innerExprs <- between beforeArgs afterArgs $ sepBy expr separator
        _ <- many spaceOrEol
        pure $ { op : f1ts op, args : fromFoldable innerExprs }
    _ <- optional $ try $ char ';'
    pure $ Chain
        { subj : f1ts <$> mbSubj
        , startOp : f1ts startOp
        , args : fromFoldable args
        , tail : fromFoldable tail
        }


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
    pure $ FnInline { args : fromCharArray args, code : f1ts inner }


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
    deeper (Chain { subj, startOp, args, tail }) =
      f $ Chain { subj, startOp, args : deeper <$> args, tail : mapSubOp deeper <$> tail }
    deeper otherExpr = f otherExpr
    mapSubOp f { op, args } = { op, args : f <$> args }


replacements :: Array (Pattern /\ Replacement)
replacements = -- this helps to fix bracket parsing in `FnInline` without specifically parsing any contents of functions' code
  [ Pattern "sin(time)" /\ Replacement "sin{[time]}"
  ]


prepare :: String -> String
prepare str = foldr (uncurry String.replaceAll) str replacements


fixback :: Script -> Script
fixback = traverseExprs fixExpr
  where
    fixExpr (FnInline { args, code }) =
      FnInline { args, code : foldr (uncurry String.replaceAll) code $ swapPR <$> replacements }
    fixExpr otherExpr = otherExpr
    swapPR (Pattern pattern /\ Replacement replacement) =
      Pattern replacement /\ Replacement pattern


instance Show Expr where
  show = case _ of
    Token str -> "%" <> str <> "%"
    Num num -> show num
    Chain { subj, startOp, args, tail } ->
      startOp <> " " <> String.joinWith " @ " (show <$> args) <> "-->" <>
      (String.joinWith " @@ " $
        (\{ op, args } ->
          "." <> op <> " " <> String.joinWith " @ " (show <$> args)
        ) <$> tail
      )
    FnInline { args, code } ->
      ">> " <> args <> " -> " <> code <> " <<"
    Comment str ->
      "// " <> str <> " //"
    EmptyLine ->
      "***"


instance Show Script where
  show :: Script -> String
  show (Script exprs) =
    String.joinWith "\n" $ show <$> exprs


data ExprLevel
  = Top
  | Arg
  | Tail Int


derive instance Eq ExprLevel


newtype LeveledExpr = LeveledExpr (ExprLevel /\ Expr)


level :: ExprLevel -> Expr -> LeveledExpr
level = curry LeveledExpr


exprFrom :: LeveledExpr -> Expr
exprFrom (LeveledExpr (_ /\ ex)) = ex


instance ToCode PS Script where
  toCode :: Proxy PS -> Script -> String
  toCode _ (Script exprs) = String.joinWith "\n" $ toCode pureScript <$> level Top <$> exprs


instance ToCode PS LeveledExpr where
  toCode :: Proxy PS -> LeveledExpr -> String
  toCode _ = case _ of
    LeveledExpr (_ /\ Token str) -> str
    LeveledExpr (_ /\ Num num) -> "n " <> show num <> ""
    LeveledExpr (_ /\ FnInline { args, code }) -> "fn $ \\_ -> {- " <> friendlyArgs args <> " -> " <> code <> " -}"
    LeveledExpr (l /\ Chain { subj, startOp, args, tail }) ->
      let
        indent =
          case l of
            Top -> ""
            Arg -> "    "
            Tail _ -> "    "
        tailLevel = case l of
            Top -> Tail 0
            Arg -> Tail 0
            Tail n -> Tail $ n + 1
      in ( if Array.length args == 1 && l == Top then
        String.joinWith "" (wrapIfNeeded <$> level Arg <$> args) <> " # " <> startOp
      else if Array.length args /= 0 then
          startOp <> " " <> String.joinWith " " (wrapIfNeeded <$> level Arg <$> args)
      else
          startOp
      )
      <>
      ( if Array.length tail > 0 then
          "\n" <> String.joinWith "\n" ((\op -> indent <> "# " <> (toCode pureScript $ subOpInChain tailLevel op)) <$> tail) <> "\n"
        else
          ""
      )
    LeveledExpr (_ /\ Comment text) -> "-- " <> text
    LeveledExpr (_ /\ EmptyLine) -> "\n\n"
    where
      wrapIfNeeded arg =
        case exprFrom arg of
          Num _ -> "(" <> toCode pureScript arg <> ")"
          FnInline _ -> "(" <> toCode pureScript arg <> ")"
          Chain _ -> "(\n" <> "    " <> toCode pureScript arg <> ")"
          _ -> toCode pureScript arg
      subOpInChain tl { op, args } = level tl $ Chain { subj : Nothing, startOp : op, args, tail : [] }
      friendlyArgs args = if String.null args then "_"  else args


instance ToCode JS Script where
  toCode :: Proxy JS -> Script -> String
  toCode _ (Script exprs) = String.joinWith "\n" $ toCode javaScript <$> level Top <$> exprs


instance ToCode JS LeveledExpr where
  toCode :: Proxy JS -> LeveledExpr -> String
  toCode _ = case _ of
    LeveledExpr (_ /\ Token str) -> str
    LeveledExpr (_ /\ Num num) -> show num
    LeveledExpr (_ /\ FnInline { args, code }) -> "(" <> args <> ")=>" <> code
    LeveledExpr (l /\ Chain { subj, startOp, args, tail }) ->
      let
        indent =
          case l of
            Top -> ""
            Arg -> "    "
            Tail _ -> "    "
        tailLevel = case l of
            Top -> Tail 0
            Arg -> Tail 0
            Tail n -> Tail $ n + 1
      in ( if Array.length args /= 0 then
          startOp <> "(" <> String.joinWith "," (toCode javaScript <$> level Arg <$> args) <> ")"
        else
          startOp <> "()"
      )
      <>
      ( if Array.length tail > 0 then
          "\n" <> String.joinWith "\n" ((\op -> indent <> "." <> (toCode javaScript $ subOpInChain tailLevel op)) <$> tail)
        else
          ""
      )
      <> (case l of
        Top -> ";\n"
        _ -> ""
      )
    LeveledExpr (_ /\ Comment text) -> "// " <> text
    LeveledExpr (_ /\ EmptyLine) -> "\n\n"
    where
      subOpInChain tl { op, args } = level tl $ Chain { subj : Nothing, startOp : op, args, tail : [] }
