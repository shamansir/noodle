module Noodle.Text.QuickDefParser where

import Prelude

import Effect.Console (log) as Console
import Effect.Class (liftEffect)

import Data.List (List)
import Data.String.CodeUnits as StringCU
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Either (Either(..))

import Control.Alt ((<|>))
import Data.Traversable (for)

import Parsing (ParserT, Parser, runParser, ParseError) as P
import Parsing.String (char, string, anyChar)  as P
import Parsing.String.Basic (noneOf) as P
import Parsing.Token (alphaNum, space) as P
import Parsing.Combinators (between, choice, option, optionMaybe, sepBy, lookAhead) as P
import Parsing.Combinators ((<?>))


import Noodle.Text.QuickDef as QD


-- type Parser m a = Functor m => Monad m => P.ParserT String m a
-- type Parser a = P.Parser String a


fnParser :: P.Parser String QD.QFamily
fnParser = do
  tag <- alphaNumToken <?> "tag"
  _ <- sep $ P.char ':'
  family <- alphaNumToken <?> "family"
  _ <- sep $ P.string "::"
  inputs <- P.option [] channels <?> "inputs"
  _ <- sep $ P.string "=>"
  outputs <- results
  _ <- Array.many P.space
  maybeImpl <- P.optionMaybe $ P.between
                (P.string "/-|")
                (P.string "|-/")
                anything
                -- (anythingExcept "|-")
  pure $ QD.qfmoi tag family inputs outputs maybeImpl


results :: P.Parser String (Array (Maybe QD.Channel))
results =
  P.choice
      [ Array.singleton <$> QD.qout <$> do
          type_ <- alphaNumToken
          _ <- Array.many P.space
          maybeDefault <- P.optionMaybe $ P.between
                        (P.char '{')
                        (P.char '}')
                        defaultValue
          pure { type : type_, maybeDefault }
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

maybeChannel :: P.Parser String (Maybe QD.Channel)
maybeChannel =
  P.choice
        [ P.char '?' *> pure Nothing
        , Just <$> do
            name <- alphaNumToken
            maybeType <- P.optionMaybe $ P.char ':' *> alphaNumToken
            _ <- Array.many P.space
            maybeDefault <- P.optionMaybe
                              -- $ Array.many P.space *>
                              $ P.between
                                (P.char '{')
                                (P.char '}')
                                defaultValue
            pure { name, type : maybeType, default : maybeDefault }
        ]


channels :: P.Parser String (Array (Maybe QD.Channel))
channels =
  P.between
        (P.char '<')
        (P.char '>')
        $ asArray
        $ P.sepBy maybeChannel arrowSep


-- channelList = forall (m :: Type -> Type). Functor m => Monad m => P.ParserT String m (Array QD.QFamily)

asArray :: forall s a. P.Parser s (List a) -> P.Parser s (Array a)
asArray = map Array.fromFoldable


-- fromList :: forall m. String -> P.Parser String (Array QD.QFamily)
-- TODO: familyList :: String -> Either (String /\ P.ParseError) (Array QD.QFamily)
familyList :: String -> Either (P.ParseError) (Array QD.QFamily)
familyList lines =
  for (String.split (String.Pattern "\n") lines) $ \s -> P.runParser s fnParser


familyList' :: String -> Array QD.QFamily
familyList' lines =
  case familyList lines of
    Left _ -> []
    Right items -> items


-- familyList :: P.Parser String (Array QD.QFamily)
-- familyList = asArray $ P.sepBy fnParser $ P.char '\n'
-- familyList = Array.many $ fnParser >>= \fn -> P.char '\n' *> pure fn