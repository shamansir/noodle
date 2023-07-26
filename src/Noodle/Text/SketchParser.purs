module Noodle.Text.SketchParser where

import Prelude

import Type.Proxy (Proxy)

-- import TryPureScript (h1, h3, p, text, render, code)
import Data.Semigroup.Foldable (class Foldable1)
import Data.CodePoint.Unicode as U
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Utils (lines) as String
import Data.Array as Array
import Data.Foldable (foldr)
import Data.String.Pattern
import Data.Tuple (curry, uncurry)
import Data.Maybe (Maybe(..), isJust)

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


data Level
  = Top
  | Arg
  | Tail


derive instance Eq Level


data Expr
    = Token String
    | Num Number
    | Chain
        Level
        { subj :: Maybe String
        , startOp :: String
        , args :: Array Expr
        , tail :: Array { op :: String, args :: Array Expr }
        }
    | FnInline { args :: String, code :: String }
    | Comment String
    | EmptyLine


derive instance Eq Expr


type ModuleName = String


data Script = Script ModuleName (Array Expr)


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


spaces :: Parser String (Array Char)
spaces = many space


breaksAndSpaces :: Parser String (Array Char)
breaksAndSpaces = many spaceOrEol


token :: Parser String (NonEmptyList Char)
token = many1 tokenChar


maybeToken :: Parser String (Array Char)
maybeToken = many tokenChar


comment :: Parser String Expr
comment = do
    _ <- spaces
    _ <- string "//"
    _ <- spaces
    commentText <- many1Till anyChar eol
    pure $ Comment $ f1ts commentText


numberx :: Parser String Expr
numberx = do
    _ <- spaces
    num <- number
    _ <- spaces
    pure $ Num num
  {- do
    idigits <- many digit
    _ <- optional $ char '.'
    fldigits <- many1 digit
    pure $ Num (fromFoldable idigits) (fromFoldable fldigits) -}


beforeArgs :: Parser String Unit
beforeArgs = do
  _ <- breaksAndSpaces
  _ <- char '('
  _ <- breaksAndSpaces
  pure unit


afterArgs :: Parser String Unit
afterArgs = do
  _ <- breaksAndSpaces
  _ <- char ')'
  _ <- breaksAndSpaces
  pure unit


separator :: Parser String Unit
separator = do
  _ <- breaksAndSpaces
  _ <- char ','
  _ <- breaksAndSpaces
  pure unit


chain :: Level -> Parser String Expr
chain level = do
    _ <- breaksAndSpaces
    mbSubj <- optionMaybe $ try $ do
        subj <- token
        _ <- spaces
        _ <- char '.'
        pure subj
    startOp <- token
    args <- between beforeArgs afterArgs $ sepBy (expr Arg) separator
    tail <- option [] $ many $ do
        _ <- breaksAndSpaces
        _ <- char '.'
        op <- token
        innerExprs <- between beforeArgs afterArgs $ sepBy (expr Tail) separator
        _ <- breaksAndSpaces
        pure $ { op : f1ts op, args : fromFoldable innerExprs }
    _ <- optional $ try $ char ';'
    pure $ Chain
        level
        { subj : f1ts <$> mbSubj
        , startOp : f1ts startOp
        , args : fromFoldable args
        , tail : fromFoldable tail
        }


fnInline :: Parser String Expr
fnInline = do
    _ <- char '('
    _ <- spaces
    args <- maybeToken
    _ <- spaces
    _ <- char ')'
    _ <- spaces
    _ <- string "=>"
    _ <- spaces
    inner <- many1 $ satisfy $ \c -> c /= ',' && c /= ')'
    _ <- spaces
    pure $ FnInline { args : fromCharArray args, code : f1ts inner }


emptyLine :: Parser String Expr
emptyLine = do
    _ <- spaces
    eol
    _ <- optional eol
    pure $ EmptyLine


tokenExpr :: Parser String Expr
tokenExpr = do
  _ <- spaces
  t <- token
  _ <- spaces
  pure $ Token $ f1ts t


expr :: Level -> Parser String Expr
expr level =
  try comment
  <|> try fnInline
  <|> try numberx
  <|> try (defer \_ -> chain level)
  <|> try tokenExpr
  <|> try emptyLine


script :: ModuleName -> Parser String Script
script mn = do
  lines <- many1 $ expr Top
  pure $ Script mn $ fromFoldable lines


traverseExprs :: (Expr -> Expr) -> Script -> Script
traverseExprs f (Script moduleName exprs) =
  Script moduleName $ deeper <$> exprs
  where
    deeper :: Expr -> Expr
    deeper (Chain l { subj, startOp, args, tail }) =
      f $ Chain l { subj, startOp, args : deeper <$> args, tail : mapSubOp deeper <$> tail }
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
    Token str -> "Token [" <> str <> "]"
    Num num -> "Num [" <> show num <> "]"
    Chain _ { subj, startOp, args, tail } ->
      "Chain "
        <> case subj of
          Just s -> "{" <> s <> "} "
          Nothing -> ""
        <> startOp
        <> " [ " <> String.joinWith " , " (show <$> args) <> " ] [ " <>
        (String.joinWith " , " $
          (\{ op, args } ->
            "Next " <> op <> " [ " <> String.joinWith " , " (show <$> args) <> " ] "
          ) <$> tail
        ) <> " ]"
    FnInline { args, code } ->
      "Fn [ " <> args <> " ] [ " <> code <> " ]"
    Comment str ->
      "Comment [" <> str <> " ]"
    EmptyLine ->
      "Empty"


instance Show Script where
  show :: Script -> String
  show (Script _ exprs) =
    String.joinWith "\n" $ show <$> exprs


instance ToCode PS Script where
  toCode :: Proxy PS -> Script -> String
  toCode _ (Script moduleName exprs) =
    (String.joinWith "\n" $ pursHeader moduleName) <> "\n\n" <>
    (String.joinWith "\n" pursPrefix) <> "\n    " <>
    (String.joinWith "\n    " $ Array.concat $ String.lines <$> toCode pureScript <$> exprs)


instance ToCode PS Expr where
  toCode :: Proxy PS -> Expr -> String
  toCode _ = case _ of
    Token str -> str
    Num num -> "n " <> show num <> ""
    FnInline { args, code } -> "fn $ \\_ -> {- " <> friendlyArgs args <> " -> " <> String.trim code <> " -}"
    Chain l { subj, startOp, args, tail } ->
      let
        indent =
          case l of
            Top -> ""
            Arg -> "    "
            Tail -> "    "
      in
      (case subj of
        Just s -> s <> " # "
        Nothing -> ""
      )
      <>
      ( if Array.length args == 1 && l == Top && not (isJust subj) && not (isNumber 0 args) then
        String.joinWith "" (wrapIfNeeded <$> args) <> " # " <> startOp
      else if Array.length args /= 0 then
          startOp <> " " <> String.joinWith " " (wrapIfNeeded <$> args)
      else
          startOp
      )
      <>
      ( if Array.length tail > 0 then
          "\n" <> String.joinWith "\n" ((\op -> indent <> "# " <> (toCode pureScript $ subOpInChain op)) <$> tail) <> "\n"
        else
          (case subj of
            Just s -> "\n"
            Nothing -> ""
          )
      )
    Comment text -> "-- " <> text
    EmptyLine -> "\n\n"
    where
      wrapIfNeeded arg =
        case arg of
          Num _ -> "(" <> toCode pureScript arg <> ")"
          FnInline _ -> "(" <> toCode pureScript arg <> ")"
          Chain _ c ->
            if Array.length c.tail > 0 then
               "(\n" <> "    " <> toCode pureScript arg <> ")"
            else
              "(" <> toCode pureScript arg <> ")"
          _ -> toCode pureScript arg
      isNumber n args =
        case Array.index args n of
          Just (Num _) -> true
          _ -> false
      subOpInChain { op, args } = Chain Tail { subj : Nothing, startOp : op, args, tail : [] }
      friendlyArgs args = if String.null args then "_"  else args


instance ToCode JS Script where
  toCode :: Proxy JS -> Script -> String
  toCode _ (Script _ exprs) = String.joinWith "\n" $ toCode javaScript <$> exprs


instance ToCode JS Expr where
  toCode :: Proxy JS -> Expr -> String
  toCode _ = case _ of
    Token str -> str
    Num num -> show num
    FnInline { args, code } -> "(" <> args <> ")=>" <> code
    Chain l { subj, startOp, args, tail } ->
      let
        tailIndent =
          case l of
            Top -> ""
            Arg -> "    "
            Tail -> "    "
        firstIndent =
          case l of
            Top -> ""
            Arg -> "\n"
            Tail -> ""
      in
      (case subj of
        Just s -> s <> "."
        Nothing -> ""
      )
      <>
      ( if Array.length args /= 0 then
          firstIndent <> startOp <> "(" <> String.joinWith "," (toCode javaScript <$> args) <> ")"
        else
          firstIndent <> startOp <> "()"
      )
      <>
      ( if Array.length tail > 0 then
          "\n" <> String.joinWith "\n" ((\op -> tailIndent <> "." <> (toCode javaScript $ subOpInChain op)) <$> tail)
        else
          ""
      )
      <> (case l of
        Top -> ";\n"
        _ -> ""
      )
    Comment text -> "// " <> text
    EmptyLine -> "\n\n"
    where
      subOpInChain { op, args } = Chain Tail { subj : Nothing, startOp : op, args, tail : [] }



pursHeader ∷ String → Array String
pursHeader moduleName =
  [ "module " <> moduleName <> " where"
  , ""
  , "import Prelude (Unit, discard, (#), ($))"
  , "import Prelude (show) as Core"
  , ""
  , "import Effect (Effect)"
  , "import Effect.Console as Console"
  , ""
  , "import Toolkit.Hydra2.Lang"
  , "import Toolkit.Hydra2.Lang.ToCode (toCode, pureScript, javaScript)"
  , "import Toolkit.Hydra2.Lang.Api"
  ]


pursPrefix :: Array String
pursPrefix =
  [ "example :: Program Unit"
  , "example = do"
  ]