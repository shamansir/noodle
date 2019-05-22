module Rpd.Parser.RpdFile where

import Prelude

import Data.Either (Either(..))
import Control.Alt ((<|>))

import Data.Array (fromFoldable)
import Data.String as String
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Either (Either(..))
import Data.List (List(..))
import Data.List.NonEmpty as NEL
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

-- import Rpd.Command (Command)
-- import Rpd.Command as Cmd
import Rpd.Toolkit as T
import Rpd.Path as P


type RpdFile = List RpdFileCommand


data RpdFileCommand
    = AddPatch P.ToPatch
    | AddNode P.ToNode T.NodeDefAlias
    | AddInlet P.ToInlet T.ChannelDefAlias
    | AddOutlet P.ToOutlet T.ChannelDefAlias
    | Connect P.ToOutlet P.ToInlet
    -- Send P.ToInlet (CanParse d => d)


nextChars :: Parser String
nextChars =
    loadString <$> many1 anyChar


delim :: Parser Unit
delim = (many1 $ satisfy isSpace) $> unit


newline :: Parser Unit
newline = (many1 $ satisfy ((==) '\n')) $> unit


slash :: Parser Unit
slash = satisfy ((==) '/') $> unit


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


addNode :: Parser RpdFileCommand
addNode = do
    _ <- string "node"
    delim
    patchAlias <- nextChars
    slash
    nodeAlias <- nextChars
    delim
    nodeDefAlias <- nextChars
    pure $ AddNode (P.toNode patchAlias nodeAlias) (T.NodeDefAlias nodeDefAlias)


addPatch :: forall d. Parser RpdFileCommand
addPatch = do
    _ <- string "patch"
    delim
    patchAlias <- nextChars
    pure $ AddPatch (P.toPatch patchAlias)



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


-- parser :: forall d. T.Toolkit d -> Parser RpdFileCommand
-- parser toolkit
--      =  addNode toolkit
--     <|> addPatch toolkit


cmdParser :: Parser RpdFileCommand
cmdParser
     =  addNode
    <|> addPatch


fileParser :: Parser RpdFile
fileParser =
    NEL.toList <$> (many1 $ do
        cmd <- cmdParser
        newline
        pure cmd)


-- TODO: use toolkit to load the appropriate node etc.
parse :: forall d c. String -> T.Toolkit d c -> Either ParseError RpdFile
parse src _ = runParser fileParser src


-- parse' :: String -> Either ParseError StringCommand
-- parse' = runParser parser'
