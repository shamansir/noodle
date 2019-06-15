module Rpd.Parser.RpdFile where

import Prelude

import Control.Alt ((<|>))

import Data.Array (fromFoldable)
import Data.String as String
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.List (List)
import Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmptyList)
import Data.Char.Unicode (isSpace, isDigit, isAlphaNum)

import Text.Parsing.StringParser (Parser, ParseError, runParser, fail)
import Text.Parsing.StringParser.CodePoints (satisfy, string)
    -- (anyDigit, eof, string, anyChar, regex, satisfy, skipSpaces)
import Text.Parsing.StringParser.Combinators (many1)
    -- (many1, endBy1, sepBy1, optionMaybe, many, manyTill, many1Till, chainl, fix, between)
-- import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)
-- import Text.Parsing.Parser.Token (space)

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


instance rpdCommandEq :: Eq RpdFileCommand where
    eq _ _ = true


instance rpdCommandShow :: Show RpdFileCommand where
    show (AddPatch patchPath) = "Add Patch: " <> show patchPath
    show (AddNode nodePath nodeDef) = "Add Node " <> show nodeDef <> ": " <> show nodePath
    show (AddInlet inletPath channelDef) =
        "Add Inlet " <> show channelDef <> ": " <> show inletPath
    show (AddOutlet outletPath channelDef) =
        "Add Outlet " <> show channelDef <> ": " <> show outletPath
    show (Connect outletPath inletPath) =
        "Connect: " <> show outletPath <> " to " <> show inletPath


isTokenChar :: Char -> Boolean
isTokenChar c | isAlphaNum c = true
isTokenChar c | c == '_'     = true
isTokenChar c | c == '-'     = true
isTokenChar c | otherwise    = false


nextToken :: Parser String
nextToken =
    loadString <$> (many1 $ satisfy isTokenChar)


delim :: Parser Unit
delim = (many1 $ satisfy isSpace) $> unit


newline :: Parser Unit
newline = (many1 $ satisfy ((==) '\n')) $> unit


slash :: Parser Unit
slash = (satisfy ((==) '/')) $> unit


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


addPatch :: Parser RpdFileCommand
addPatch = do
    _ <- string "patch"
    delim
    patchAlias <- nextToken
    pure $ AddPatch (P.toPatch patchAlias)


addNode :: Parser RpdFileCommand
addNode = do
    _ <- string "node"
    delim
    patchAlias <- nextToken
    slash
    nodeAlias <- nextToken
    delim
    nodeDefAlias <- nextToken
    pure $ AddNode (P.toNode patchAlias nodeAlias) (T.NodeDefAlias nodeDefAlias)


addInlet :: Parser RpdFileCommand
addInlet = do
    _ <- string "inlet"
    delim
    patchAlias <- nextToken
    slash
    nodeAlias <- nextToken
    slash
    inletAlias <- nextToken
    delim
    channelDefAlias <- nextToken
    pure $ AddInlet
        (P.toInlet patchAlias nodeAlias inletAlias)
        (T.ChannelDefAlias channelDefAlias)


addOutlet :: Parser RpdFileCommand
addOutlet = do
    _ <- string "outlet"
    delim
    patchAlias <- nextToken
    slash
    nodeAlias <- nextToken
    slash
    outletAlias <- nextToken
    delim
    channelDefAlias <- nextToken
    pure $ AddOutlet
        (P.toOutlet patchAlias nodeAlias outletAlias)
        (T.ChannelDefAlias channelDefAlias)


connect :: Parser RpdFileCommand
connect = do
    _ <- string "connect"
    delim
    outetPatchAlias <- nextToken
    slash
    outletNodeAlias <- nextToken
    slash
    outletAlias <- nextToken
    delim
    inletPatchAlias <- nextToken
    slash
    inletNodeAlias <- nextToken
    slash
    inletAlias <- nextToken
    pure $ Connect
        (P.toOutlet outetPatchAlias outletNodeAlias outletAlias)
        (P.toInlet inletPatchAlias inletNodeAlias inletAlias)


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
     =  addPatch
    <|> addNode
    <|> addInlet
    <|> addOutlet
    <|> connect


fileParser :: Parser RpdFile
fileParser = do
    NEL.toList <$> (many1 $ do
        cmd <- cmdParser
        newline
        pure cmd)


-- TODO: use toolkit to load the appropriate node etc.
parse :: forall d c n. String -> T.Toolkit d c n -> Either ParseError RpdFile
parse src _ = runParser fileParser src


-- parse' :: String -> Either ParseError StringCommand
-- parse' = runParser parser'
