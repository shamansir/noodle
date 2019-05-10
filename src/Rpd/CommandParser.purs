module Rpd.CommandParser where

import Prelude

import Data.Either (Either(..))
import Control.Alt ((<|>))

import Data.Array (fromFoldable)
import Data.String as String
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Char.Unicode (isSpace, isDigit)
import Data.Lens ((^.))
import Data.Lens.At (at)

import Control.Monad.Trans.Class (class MonadTrans, lift)

import Text.Parsing.StringParser (Parser, ParseError, runParser, fail)
import Text.Parsing.StringParser.CodePoints
    (anyDigit, eof, string, anyChar, regex, satisfy, skipSpaces)
import Text.Parsing.StringParser.Combinators (many1, endBy1, sepBy1, optionMaybe, many, manyTill, many1Till, chainl, fix, between)
import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Token (space)

import Rpd.Command (Command, StringCommand)
import Rpd.Command as Cmd
import Rpd.Toolkit as T
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


addNode :: forall d. T.Toolkit d -> Parser (Maybe (Command d))
addNode toolkit =
    addNode' >>=
        \command ->
            pure $ case command of
                (Cmd.AddNode' patchPath name) ->
                    T.findNodeDef name toolkit
                        <#> Cmd.AddNode patchPath (P.mkAlias name)
                _ -> Nothing


-- addNode toolkit = do
    -- command <- addNode'
    -- pure $ case command of
    --     (Cmd.AddNode' patchPath name) ->
    --         let node = ?wh
    --         in Just $ Cmd.AddNode patchPath node
    --     _ -> Nothing


addNode' :: Parser StringCommand
addNode' = do
    _ <- string "node"
    delim
    patchAlias <- nextChars
    delim
    nodeDefAlias <- nextChars
    pure $ Cmd.AddNode' (P.PatchPath $ P.mkAlias patchAlias) nodeDefAlias


addPatch :: forall d. T.Toolkit d -> Parser (Maybe (Command d))
addPatch toolkit
    = string "patch" $> Just Cmd.Bang


addPatch' :: Parser StringCommand
addPatch'
    = string "patch" $> Cmd.Bang'



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


parser :: forall d. T.Toolkit d -> Parser (Maybe (Command d))
parser toolkit
     =  addNode toolkit
    <|> addPatch toolkit


parser' :: Parser StringCommand
parser'
     =  addNode'
    <|> addPatch'


-- TODO: merge Nothing with ParseError
parse :: forall d. String -> T.Toolkit d -> Either ParseError (Maybe (Command d))
parse = flip (runParser <<< parser)


parse' :: String -> Either ParseError StringCommand
parse' = runParser parser'
