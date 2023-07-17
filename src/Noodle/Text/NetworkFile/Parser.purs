module Noodle.Text.NetworkFile.Parser where

import Prelude

import Data.Foldable (fold)
import Effect (Effect)
import Parsing (Parser, runParser)
import Parsing.String (char, string)
import Parsing.String.Basic (alphaNum, space, number)
import Parsing.Combinators (many1Till)
import Control.Alt ((<|>))
import Data.Array (many)
import Data.String as String
import Data.String.NonEmpty.CodeUnits as CU
import Data.String.NonEmpty.Internal as StringX
import Data.Tuple.Nested ((/\), type (/\))

myFile :: String
myFile =
  """hydra 0.1
osc 40 60 osc-0
pi 20 20 pi-0
number 40 40 num-0
<> pi-0 0 osc-0 0
<> num-0 0 osc-0 1
-> osc-0 0 N 20.0
~> num-0 0 N 40.0
  """

data Command
    = MakeNode String Int Int String
    | Connect String Int String Int
    | Send String Int String
    | SendO String Int String
    | Connect_ String String String String
    | Send_ String String String
    | SendO_ String String String


derive instance Eq Command


newtype Header = Header (String /\ Number)

data Program = Program Header (Array Command)


alphaNumTill :: forall a. Parser String a -> Parser String String
alphaNumTill stopAt =
  many1Till alphaNum stopAt <#> CU.fromFoldable1 <#> StringX.toString


eol :: Parser String Unit
eol = char '\n' *> pure unit


parser :: Parser String Program
parser = do
  toolkit <- alphaNumTill space
  version <- number
  eol
  _ <- string "osc"
  pure $ Program (Header $ toolkit /\ version) []


class ToCode x where
  toCode :: x -> String


instance ToCode Command where
    toCode :: Command -> String
    toCode =
        case _ of
            -- Header toolkit version -> toolkit <> " " <> version
            MakeNode family top left nodeId -> family <> " " <> show top <> " " <> show left <> " " <> nodeId
            Connect fromNode oindex toNode iindex -> "<> " <> fromNode <> " " <> show oindex <> " " <> toNode <> " " <> show iindex
            Send nodeId iindex value -> "-> " <> nodeId <> " " <> show iindex <> " " <> value
            SendO nodeId oindex value -> "~> " <> nodeId <> " " <> show oindex <> " " <> value
            Connect_ fromNode oname toNode iname -> "<> " <> fromNode <> " " <> oname <> " " <> toNode <> " " <> iname
            Send_ nodeId iname value -> "-> " <> nodeId <> " " <> iname <> " " <> value
            SendO_ nodeId oname value -> "~> " <> nodeId <> " " <> oname <> " " <> value


instance Show Program where
  show :: Program -> String
  show (Program (Header (tk /\ version)) commands) =
    tk <> " " <> show version <> "\n" <>
    (String.joinWith "\n" $ toCode <$> commands)

{-
import Data.Array (fromFoldable, (:))
import Parsing (Parser, runParser)
import Parsing.Combinators (skipMany, try)
import Parsing.Combinators.Array (many1)
import Parsing.String (anyChar, char, string)
import Parsing.String.Basic (skipSpaces, letter, digit, number, intDecimal)
import Control.Alt ((<|>))
import Data.String.NonEmpty.CodeUnits as CU
import Data.String.NonEmpty.Internal as String
import Data.Int as Int
import Data.Maybe (fromMaybe)
-- import Read (read)

-- Types for different commands
data NodeType = Osc | Pi | Number
-- derive instance eqNodeType :: Eq NodeType
-- derive instance showNodeType :: Show NodeType

data Command
  = CreateNode NodeType Int Int String
  | ConnectNodes String Int String Int
  | SendDataToInput String Int Number
  | SendDataToOutput String Int Number


-- derive instance eqCommand :: Eq Command
-- derive instance showCommand :: Show Command


-- Helper functions
parseNodeType :: Parser String NodeType
parseNodeType =
  try (string "osc" *> pure Osc)
  <|> try (string "pi" *> pure Pi)
  <|> try (string "number" *> pure Number)


-- parseInt :: Parser String Int
-- parseInt = map (CU.fromNonEmptyCharArray >>> String.toString >>> Int.fromString >>> fromMaybe 0) $ many1 digit


-- parseFloat :: Parser String Number
-- parseFloat = map ?wh $ (<>) <$> many1 digit <*> ((:) <$> char '.' <*> many1 digit)


parseCommand :: Parser String Command
parseCommand =
  try (char '<' *> char '>' *>
       ConnectNodes <$> (skipSpaces *> many1 letter <* skipSpaces)
                    <*> (intDecimal <* skipSpaces)
                    <*> (many1 letter <* skipSpaces)
                    <*> intDecimal)
  <|> try (char '~' *> char '>' *>
           SendDataToOutput <$> (skipSpaces *> many1 letter <* skipSpaces)
                            <*> (intDecimal <* skipSpaces <* char 'N')
                            <*> number)
  <|> try (char '~' *> char '>' *>
           SendDataToInput <$> (skipSpaces *> many1 letter <* skipSpaces)
                           <*> (intDecimal <* skipSpaces <* char 'N')
                           <*> number)
  <|> try (CreateNode <$> (skipSpaces *> parseNodeType <* skipSpaces)
                      <*> (intDecimal <* skipSpaces)
                      <*> (intDecimal <* skipSpaces)
                      <*> many1 letter)


parseFile :: Parser (Array Command)
parseFile = skipMany (char ' ') *> many1 parseCommand <* skipMany (char ' ')
-}
