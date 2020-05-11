module Xodus.QueryParser where

import Prelude

import Control.Alt ((<|>))

import Data.Array (fromFoldable)
import Data.Char.Unicode (isSpace, isDigit, isAlphaNum)
import Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmptyList)
import Data.String as String
import Data.Int (fromString)

import Text.Parsing.StringParser (Parser, ParseError, runParser, fail)
import Text.Parsing.StringParser.CodePoints (satisfy, string)
    -- (anyDigit, eof, string, anyChar, regex, satisfy, skipSpaces)
import Text.Parsing.StringParser.Combinators (many1)
    -- (many1, endBy1, sepBy1, optionMaybe, many, manyTill, many1Till, chainl, fix, between)
-- import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)
-- import Text.Parsing.Parser.Token (space)

import Xodus.Dto
import Xodus.Query


data SortDirection
    = Ascending
    | Descending


isTokenChar :: Char -> Boolean
isTokenChar c | isAlphaNum c = true
isTokenChar c | c == '_'     = true
isTokenChar c | c == '-'     = true
isTokenChar c | otherwise    = false


cOperator :: forall a. Eq a => Ord a => Parser (a -> a -> Boolean)
cOperator =
        string "is" $> (==)
    <|> string "==" $> (==)
    <|> string "<" $> (<)
    <|> string ">" $> (>)
    <|> string ">=" $> (>=)
    <|> string "<=" $> (<=)


loadString :: NonEmptyList Char -> String
loadString list =
    String.fromCodePointArray
        $ map String.codePointFromChar
        $ fromFoldable list


nextToken :: Parser String
nextToken =
    loadString <$> (many1 $ satisfy isTokenChar)


delim :: Parser Unit
delim = (many1 $ satisfy isSpace) $> unit


direction :: Parser SortDirection
direction =
        string "asc" $> Ascending
    <|> string "desc" $> Descending


condition :: Parser Condition
condition = do
    fieldName <- nextToken
    delim
    op <- (cOperator :: Parser (String -> String -> Boolean))
    delim
    value <- nextToken
    pure $ Condition \_ -> true


comparison :: Parser Comparison
comparison = do
    fieldName <- nextToken
    pure $ Comparison \_ _ -> EQ
