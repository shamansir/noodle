module Rpd.Renderer.Terminal.CommandParser where

import Prelude

import Data.Either (Either(..))
import Control.Alt ((<|>))

import Data.Array (fromFoldable)
import Data.String as String
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Char.Unicode (isSpace, isDigit)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Text.Parsing.StringParser (Parser, ParseError, runParser, fail)
import Text.Parsing.StringParser.CodePoints
    (anyDigit, eof, string, anyChar, regex, satisfy, skipSpaces)
import Text.Parsing.StringParser.Combinators (many1, endBy1, sepBy1, optionMaybe, many, manyTill, many1Till, chainl, fix, between)
import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Token (space)

import Rpd.Command (Command)
import Rpd.Command as Cmd
import Rpd.Path as P


nextChars :: Parser String
nextChars =
    loadString <$> many1 anyChar


delim :: Parser Unit
delim = (many1 $ satisfy isSpace) $> unit


number :: Parser Int
number = do
    numStr <- many1 $ satisfy isDigit
    let maybeInt = fromString $ loadString numStr
    case maybeInt of
        Just val -> pure val
        Nothing -> fail ""


loadString :: NonEmptyList Char -> String
loadString list =
    String.fromCodePointArray
        $ map String.codePointFromChar
        $ fromFoldable list


addNode :: Parser Command
addNode = do
    _ <- string "node"
    delim
    patchId <- number
    delim
    name <- nextChars
    pure $ Cmd.AddNode (P.PatchId patchId) name


addPatch :: Parser Command
addPatch
    = string "patch" $> Cmd.Bang



-- digit = string "0" $> 0
--     <|> string "1" $> 1
--     <|> string "2" $> 2
--     <|> string "3" $> 3
--     <|> string "4" $> 4
--     <|> string "5" $> 5
--     <|> string "6" $> 6
--     <|> string "7" $> 7
--     <|> string "8" $> 8
--     <|> string "9" $> 9


parser :: Parser Command
parser = addNode
    <|> addPatch


parse :: String -> Either ParseError Command
parse = runParser parser
