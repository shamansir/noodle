module Xodus.QueryParser where

import Prelude

import Control.Alt ((<|>))

import Data.Array (fromFoldable)
import Data.Char.Unicode (isSpace, isDigit, isAlphaNum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either)
import Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmptyList)
import Data.String as String
import Data.Int (fromString) as Int
import Data.Number (fromString) as Number
import Data.Ordering (Ordering(..)) as O
import Data.Tuple.Nested ((/\), type (/\))

import Text.Parsing.StringParser (Parser, ParseError, runParser, fail)
import Text.Parsing.StringParser.CodePoints (satisfy, string)
import Text.Parsing.StringParser.Combinators (many1)


import Xodus.Dto (dataOfProperty)
import Xodus.Query
import Xodus.Query (ComparisonOp(..)) as Q


run :: forall a. Parser a -> String -> Either ParseError a
run = runParser


cOperator :: Parser ComparisonOp
cOperator =
        string "is" $> Q.EQ
    <|> string "==" $> Q.EQ
    <|> string "<"  $> Q.LT
    <|> string ">"  $> Q.GT
    <|> string ">=" $> Q.GTE
    <|> string "<=" $> Q.LTE


{-
cOperator :: forall a. Eq a => Ord a => Parser (a -> a -> Boolean)
cOperator =
        string "is" $> (==)
    <|> string "==" $> (==)
    <|> string "<"  $> (<)
    <|> string ">"  $> (>)
    <|> string ">=" $> (>=)
    <|> string "<=" $> (<=)
-}


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


condition :: Parser (ConditionInfo /\ Condition)
condition = do
    fieldName <- nextToken
    delim
    cOp <- cOperator
    delim
    reqValue <- nextToken
    pure $ makePInfo fieldName cOp reqValue /\ Condition
        \entity ->
            case dataOfProperty entity fieldName of
                Just { name, type : type_, value }
                    -> compare type_ cOp value reqValue
                Nothing
                    -> false
    where
        compare "string" cOp value reqValue =
            (toOp cOp) value reqValue
        compare "int" cOp value reqValue =
            toOp cOp <$> Int.fromString value <*> Int.fromString reqValue # fromMaybe false
        compare "float" cOp value reqValue =
            toOp cOp <$> Number.fromString value <*> Number.fromString reqValue # fromMaybe false
        compare _ _ _ _ = false


comparison :: Parser (SortInfo /\ Comparison)
comparison = do
    fieldName <- nextToken
    delim
    dir <- direction
    pure $ makeSInfo fieldName dir /\ Comparison
        \entityA entityB ->
            fromMaybe O.EQ $ do
                { type : typeA, value : valueA } <- dataOfProperty entityA fieldName
                { type : typeB, value : valueB } <- dataOfProperty entityA fieldName
                pure $ compare typeA typeB dir valueA valueB
    where
        compare "string" "string" dir valueA valueB =
            (dirToOp dir) valueA valueB
        compare "int" "int" dir valueA valueB =
            dirToOp dir
                <$> Int.fromString valueA
                <*> Int.fromString valueB
                # fromMaybe O.EQ
        compare "float" "float" dir valueA valueB =
            dirToOp dir
                <$> Number.fromString valueA
                <*> Number.fromString valueB
                # fromMaybe O.EQ
        compare _ _ _ _ _ = O.EQ


isTokenChar :: Char -> Boolean
isTokenChar c | isAlphaNum c = true
isTokenChar c | c == '_'     = true
isTokenChar c | c == '-'     = true
isTokenChar c | otherwise    = false

