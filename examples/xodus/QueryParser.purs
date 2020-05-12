module Xodus.QueryParser where

import Prelude

import Control.Alt ((<|>))

import Data.Array (fromFoldable)
import Data.Char.Unicode (isSpace, isDigit, isAlphaNum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmptyList)
import Data.String as String
import Data.Int (fromString) as Int
import Data.Number (fromString) as Number
import Data.Ordering (Ordering(..)) as O
import Data.Tuple.Nested ((/\), type (/\))

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


data ComparisonOp
    = EQ
    | LT
    | GT
    | GTE
    | LTE


data ComparisonInfo = ComparisonInfo Field ComparisonOp String
data SortInfo = SortInfo Field SortDirection


cOperator :: Parser ComparisonOp
cOperator =
        string "is" $> EQ
    <|> string "==" $> EQ
    <|> string "<"  $> LT
    <|> string ">"  $> GT
    <|> string ">=" $> GTE
    <|> string "<=" $> LTE


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


condition :: Parser (ComparisonInfo /\ Condition)
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


toOp :: forall a. Eq a => Ord a => ComparisonOp -> (a -> a -> Boolean)
toOp EQ = (==)
toOp LT = (<)
toOp GT = (>)
toOp LTE = (<=)
toOp GTE = (>=)


dirToOp :: forall a. Eq a => Ord a => SortDirection -> (a -> a -> O.Ordering)
dirToOp Ascending a b | a == b = O.EQ
dirToOp Ascending a b | a < b = O.LT
dirToOp Ascending a b | a > b = O.GT
dirToOp Ascending a b | otherwise = O.EQ
dirToOp Descending a b | a == b = O.EQ
dirToOp Descending a b | a < b = O.GT
dirToOp Descending a b | a > b = O.LT
dirToOp Descending a b | otherwise = O.EQ


makePInfo :: String -> ComparisonOp -> String -> ComparisonInfo
makePInfo f op v = ComparisonInfo (Field f) op v


makeSInfo :: String -> SortDirection -> SortInfo
makeSInfo f dir = SortInfo (Field f) dir
