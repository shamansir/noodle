module Noodle.Text.NdfFile.NodeDef.Parser where

import Prelude


import Data.List (List)
import Data.String.CodeUnits as StringCU
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))

import Data.Traversable (for)

import Parsing (Parser, runParser, ParseError) as P
import Parsing.String (char, string)  as P
import Parsing.String.Basic (noneOf) as P
import Parsing.Token (alphaNum, space) as P
import Parsing.Combinators (between, choice, option, optionMaybe, sepBy) as P
import Parsing.Combinators ((<?>))

import Noodle.Fn.ToFn (fn') as Fn
import Noodle.Text.NdfFile.NodeDef (ChannelDef, NodeDef(..), NodeFnDef(..), ProcessAssign(..))
import Noodle.Text.NdfFile.Newtypes (EncodedType(..), EncodedValue(..), FamilyGroup(..), NodeFamily(..), ProcessCode(..))


parser :: P.Parser String NodeDef
parser = do
  _ <- sep $ P.char ':'
  tag <- alphaNumToken <?> "tag"
  _ <- sep $ P.char ':'
  family <- alphaNumToken <?> "family"
  _ <- sep $ P.string "::"
  inputs <- P.option [] channels <?> "inputs"
  _ <- sep $ P.string "=>"
  outputs <- results
  _ <- Array.many P.space
  maybeImpl <- P.optionMaybe processCode
  pure $ NodeDef
    { group : FamilyGroup tag
    , family : NodeFamily family
    , fn : NodeFnDef $ Fn.fn' family (Array.catMaybes inputs) (Array.catMaybes outputs)
    , process : maybeImpl
    }


results :: P.Parser String (Array (Maybe (String /\ ChannelDef)))
results =
  P.choice
      [ Array.singleton <$> Just <$> channel
      , Array.singleton <$> do
          type_ <- alphaNumToken
          _ <- Array.many P.space
          maybeDefault <- P.optionMaybe $ P.between
                        (P.char '{')
                        (P.char '}')
                        defaultValue
          pure $ Just $ "out" /\
            { dataType : Just $ EncodedType type_
            , defaultValue : EncodedValue <$> maybeDefault
            }
      , P.option [] channels
      ] <?> "outputs"


arrowSep ∷ P.Parser String Unit
arrowSep = sep $ P.choice [ P.string "->", StringCU.singleton <$> P.char '→' ]


sep :: forall s. P.Parser String s -> P.Parser String Unit
sep sep_ = do
    _ <- Array.many P.space
    _ <- sep_
    _ <- Array.many P.space
    pure unit


anything :: P.Parser String String
anything = StringCU.fromCharArray <$> Array.some (P.noneOf [ '|' ])

alphaNumToken :: P.Parser String String
alphaNumToken = StringCU.fromCharArray <$> Array.some P.alphaNum

defaultValue :: P.Parser String String
defaultValue = StringCU.fromCharArray <$> Array.some (P.choice [ P.alphaNum, P.char '.', P.char ' ', P.char '_' ])

channel :: P.Parser String (String /\ ChannelDef)
channel = do
  name <- alphaNumToken
  maybeType <- P.optionMaybe $ P.char ':' *> alphaNumToken
  _ <- Array.many P.space
  maybeDefault <- P.optionMaybe
                    -- $ Array.many P.space *>
                    $ P.between
                      (P.char '{')
                      (P.char '}')
                      defaultValue
  pure (name /\
      { dataType : EncodedType <$> maybeType
      , defaultValue : EncodedValue <$> maybeDefault
      }
  )

maybeChannel :: P.Parser String (Maybe (String /\ ChannelDef))
maybeChannel =
  P.choice
        [ P.char '?' *> pure Nothing
        , Just <$> channel
        ]


processCode :: P.Parser String ProcessCode
processCode =
  P.between
      (P.string "/-|")
      (P.string "|-/")
      anything
  <#> ProcessCode


channels :: P.Parser String (Array (Maybe (String /\ ChannelDef)))
channels =
  P.between
        (P.char '<')
        (P.char '>')
        $ asArray
        $ P.sepBy maybeChannel arrowSep


assignmentParser :: P.Parser String ProcessAssign
assignmentParser = do
  _ <- sep $ P.char '$'
  family <- alphaNumToken <?> "family"
  _ <- sep $ P.string "::"
  processCode <- processCode
  pure $ ProcessAssign $ NodeFamily family /\ processCode



asArray :: forall s a. P.Parser s (List a) -> P.Parser s (Array a)
asArray = map Array.fromFoldable


-- TODO: return friendlier parsing errors
toolkitList :: String -> Either (P.ParseError) (Array NodeDef)
toolkitList lines =
  for (String.split (String.Pattern "\n") lines) $ \s -> P.runParser s parser


toolkitList' :: String -> Array NodeDef
toolkitList' lines =
  case toolkitList lines of
    Left _ -> []
    Right items -> items