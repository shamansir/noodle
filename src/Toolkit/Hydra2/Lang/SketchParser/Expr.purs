module Toolkit.Hydra2.Lang.SketchParser.Expr where

import Prelude

import Debug as Debug
import Type.Proxy (Proxy)

-- import TryPureScript (h1, h3, p, text, render, code)
import Data.String.CodeUnits (fromCharArray)
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (foldr)
import Data.Tuple.Nested ((/\))

import Parsing (Parser, runParser)
import Parsing.String (char, string, anyChar, satisfy)
import Parsing.String.Basic (number)
import Parsing.Combinators (choice, between, many1, many1Till, option, optionMaybe, optional, sepBy, try)

import Control.Alt ((<|>))
import Control.Lazy (defer)

import Data.Array (many, fromFoldable)
import Data.String as String

import Toolkit.Hydra2.Types (Value(..))
import Toolkit.Hydra2.Lang.ToCode (class ToCode, NDF, PS, JS, pureScript, toCode, javaScript)
import Toolkit.Hydra2.Lang.Fn (possiblyToFn, KnownFn(..), Argument(..))

import Toolkit.Hydra2.Lang.SketchParser.Utils
import Toolkit.Hydra2.Lang.SketchParser.IExpr (IExpr, inlineExprParser)


data Level
  = Top
  | Arg
  | Tail


derive instance Eq Level


data Expr
    = Token String
    | Num Number
    | Assign String Expr
    | Arr (Array Expr) (Array { op :: String, args :: Array Expr })
    | Chain
        Level
        { subj :: Maybe String
        , startOp :: String
        , args :: Array Expr
        , tail :: Array { op :: String, args :: Array Expr }
        }
    | FnInline { args :: String, code :: Either String IExpr }
    | Inline IExpr
    | Comment String
    | EmptyLine


derive instance Eq Expr


assignment :: Parser String Expr
assignment = do
    _ <- spaces
    t <- token
    _ <- spaces
    _ <- char '='
    _ <- spaces
    iexpr <- expr Top
    _ <- spaces
    pure $ Assign (f1ts t) iexpr



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
    n <- number
    _ <- spaces
    pure $ Num n


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


beforeArray :: Parser String Unit
beforeArray = do
  _ <- breaksAndSpaces
  _ <- char '['
  _ <- breaksAndSpaces
  pure unit


afterArray :: Parser String Unit
afterArray = do
  _ <- breaksAndSpaces
  _ <- char ']'
  _ <- breaksAndSpaces
  pure unit


commaSeparator :: Parser String Unit
commaSeparator = do
  _ <- breaksAndSpaces
  _ <- char ','
  _ <- breaksAndSpaces
  pure unit


array :: Parser String Expr
array = do
    elements <- between beforeArray afterArray $ sepBy (defer $ \_ -> expr Top) commaSeparator
    tail <- option [] $ many $ do
        _ <- breaksAndSpaces
        _ <- char '.'
        op <- token
        innerExprs <- between beforeArgs afterArgs $ sepBy (expr Tail) commaSeparator
        _ <- breaksAndSpaces
        pure $ { op : f1ts op, args : fromFoldable innerExprs }
    _ <- optional $ try $ char ';'
    pure $ Arr
        (fromFoldable elements)
        (fromFoldable tail)


chain :: Level -> Parser String Expr
chain level = do
    _ <- breaksAndSpaces
    mbSubj <- optionMaybe $ try $ do
        subj <- token
        _ <- spaces
        _ <- char '.'
        pure subj
    startOp <- token
    args <- between beforeArgs afterArgs $ sepBy (expr Arg) commaSeparator
    tail <- option [] $ many $ do
        _ <- breaksAndSpaces
        _ <- char '.'
        op <- token
        innerExprs <- between beforeArgs afterArgs $ sepBy (expr Tail) commaSeparator
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
    code <- choice
        [ Right <$> inlineExprParser
        , Left <$> f1ts <$> (many1 $ satisfy $ \c -> c /= ',' && c /= ')')
        ]
    -- let
    --     code' = either ?wh ?wh code
    --   code' = either (const $ Left codeStr) Right $ runParser codeStr inlineExprParser
    _ <- spaces
    -- pure $ FnInline { args : fromCharArray args, code : code' }
    pure $ FnInline { args : fromCharArray args, code }


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


inlineExpr :: Parser String Expr
inlineExpr = do
  _ <- spaces
  ie <- inlineExprParser
  _ <- spaces
  pure $ Inline ie


expr :: Level -> Parser String Expr
expr level =
  try comment
  <|> try fnInline
  <|> try numberx
  <|> try (defer \_ -> assignment)
  <|> try (defer \_ -> array)
  <|> try (defer \_ -> chain level)
  <|> try inlineExpr
  <|> try tokenExpr
  <|> try emptyLine


instance Show Expr where
  show = case _ of
    Token str -> "Token [" <> str <> "]"
    Num num -> "Num [" <> show num <> "]"
    Arr exprs tail ->
      "Array "
        <> " [ " <> String.joinWith " , " (show <$> exprs) <> " ] [ " <>
        (String.joinWith " , " $
          (\{ op, args } ->
            "Next " <> op <> " [ " <> String.joinWith " , " (show <$> args) <> " ] "
          ) <$> tail
        ) <> " ]"
    Assign name expr -> "Assign " <>  show name <> " [" <> show expr <> "]"
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
      "Fn [ " <> args <> " ] [ " <> either String.trim show code <> " ]"
    Inline iexpr ->
      "IExpr [ " <> show iexpr <> " ]"
    Comment str ->
      "Comment [" <> str <> " ]"
    EmptyLine ->
      "Empty"


instance ToCode PS Expr where
  toCode :: Proxy PS -> Expr -> String
  toCode _ = case _ of
    Token str -> str
    Num num -> "n " <> show num <> ""
    FnInline fn -> formatInlineFn fn
    Inline iexpr -> toCode pureScript iexpr
    Assign name expr -> "let " <> name <> " = " <> toCode pureScript expr
    Arr exprs tail ->
      let indent = "                      "
      in
      "[ " <>
      String.joinWith " , " ((\expr -> toCode pureScript expr) <$> exprs) <> "\n"
      <> " ]" <>
      ( if Array.length tail > 0 then
          "\n" <> String.joinWith "\n" ((\op -> indent <> "# " <> (toCode pureScript $ subOpInChain op)) <$> tail) <> "\n"
        else
          ""
      )
    Chain l { subj, startOp, args, tail } ->
      let
        indent =
          case l of
            Top -> "    "
            Arg -> "        "
            Tail -> "        "
      in
      (case subj of
        Just s -> s <> " # "
        Nothing -> ""
      )
      <>
      ( if Array.length args == 1 && l == Top && not (isJust subj) && not (isNumber 0 args) && (Array.length tail == 0) then
        String.joinWith "" (wrapIfNeeded <$> fillLackingArgs startOp args) <> " # " <> Debug.spy "for" startOp
      else if Array.length args /= 0 then
          Debug.spy "for" startOp <> " " <> String.joinWith " " (wrapIfNeeded <$> fillLackingArgs startOp args)
      else
          if startOp == "out" then "outs" else startOp
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
      formatInlineFn { args, code } =
        case code of
          Right iexpr -> "fn $ \\" <> friendlyArgs args <> " -> " <> toCode pureScript iexpr
          Left str -> "fn $ \\_ -> pure unit {- " <> friendlyArgs args <> " -> " <> String.trim str <> " -}"
      wrapIfNeeded arg =
        case arg of
          Num _ -> "(" <> toCode pureScript arg <> ")"
          FnInline _ -> "(" <> toCode pureScript arg <> ")"
          Inline _ -> "(" <> toCode pureScript arg <> ")"
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
      -- FIXME: we may alter the function name from the API instead
      valueToExpr (Number n) = Num n
      valueToExpr _ = Token "fail"  -- all the default values for Hydra arguments are numerals, so we're kinda safe here
      fillLackingArgs startOp args =
        case possiblyToFn (KnownFn startOp) of
          Just (_ /\ defArgs) ->
            if (Debug.spy "actual" $ Array.length args) < (Debug.spy "expected" $ Array.length defArgs) then
              Array.range (Array.length args) (Array.length defArgs - 1)
                # foldr
                    (\idx prevArgs ->
                      case Array.index defArgs idx of
                        Just (Argument _ defValue) -> prevArgs # Array.insertAt idx (valueToExpr defValue) # fromMaybe prevArgs
                        Nothing -> prevArgs
                    )
                  args
            else
              args
          Nothing -> args


instance ToCode JS Expr where
  toCode :: Proxy JS -> Expr -> String
  toCode _ = case _ of
    Token str -> str
    Num num -> show num
    FnInline fn -> formatInlineFn fn
    Assign name expr -> name <> " = " <> toCode pureScript expr
    Inline iexpr -> toCode javaScript iexpr
    Arr exprs tail ->
      let
        indent = "        "
      in
      "[ " <>
      (String.joinWith " , " ((\expr -> toCode pureScript expr) <$> exprs))
      <> " ]" <>
      ( if Array.length tail > 0 then
          "\n" <> String.joinWith "\n" ((\op -> indent <> "." <> (toCode javaScript $ subOpInChain op)) <$> tail)
        else
          ""
      )
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
      formatInlineFn { args, code } =
        "(" <> args <> ")=>" <> either String.trim (toCode javaScript) code
      subOpInChain { op, args } = Chain Tail { subj : Nothing, startOp : op, args, tail : [] }
