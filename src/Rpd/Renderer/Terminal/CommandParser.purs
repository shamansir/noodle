module Rpd.Renderer.Terminal.CommandParser where

import Prelude

import Data.Either (Either(..))
import Control.Alt ((<|>))

import Text.Parsing.StringParser (Parser, ParseError, runParser)
import Text.Parsing.StringParser.CodePoints (anyDigit, eof, string, anyChar, regex)


digit :: Parser Int
digit = string "0" $> 0
    <|> string "1" $> 1
    <|> string "2" $> 2
    <|> string "3" $> 3
    <|> string "4" $> 4
    <|> string "5" $> 5
    <|> string "6" $> 6
    <|> string "7" $> 7
    <|> string "8" $> 8
    <|> string "9" $> 9


parser :: Parser Int
parser = digit


parse :: String -> Either ParseError Int
parse = runParser parser
