module Noodle.Text.NdfFile.NodeDef.Parser where

import Prelude

import Control.Alt ((<|>))

import Data.List (List)
import Data.String.CodeUnits as StringCU
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))

import Data.Traversable (for)

import Parsing (Parser, runParser, ParseError) as P
import Parsing.String (char, string)  as P
import Parsing.String.Basic (noneOf) as P
import Parsing.Token (alphaNum, space) as P
import Parsing.Combinators (between, choice, option, optionMaybe, sepBy, try) as P
import Parsing.Combinators ((<?>))

import Noodle.Fn.ToFn (fn') as Fn
import Noodle.Text.NdfFile.NodeDef (NodeDef(..), NodeFnDef(..), ProcessAssign(..))
import Noodle.Text.NdfFile.NodeDef.ProcessCode (ProcessCode(..))
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..), FamilyGroup(..), NodeFamily(..), ChannelDef(..), StateDef(..), emptyStateDef)


parser :: P.Parser String NodeDef
parser = do
  _ <- sep $ P.char ':'
  tag <- alphaNumToken <?> "tag"
  _ <- sep $ P.char ':'
  family <- alphaNumToken <?> "family"
  _ <- sep $ P.string "::"
  mbState <- P.try maybeState
  _ <- Array.many P.space
  inputs <- P.option [] channels <?> "inputs"
  _ <- sep $ P.string "=>"
  outputs <- results
  _ <- Array.many P.space
  maybeImpl <- P.optionMaybe processCode
  pure $ NodeDef
    { group : FamilyGroup tag
    , state : fromMaybe emptyStateDef mbState
    , fn : NodeFnDef $ Fn.fn' family (Array.catMaybes inputs) (Array.catMaybes outputs)
    , process : fromMaybe NoneSpecified maybeImpl
    }


maybeState :: P.Parser String (Maybe StateDef)
maybeState = do
  P.option Nothing
      $ P.between (P.char '[') (P.char ']')
      $ typeAndMbDefault
            (\type_ mbDefault -> Just $ StateDef
              { mbType : Just $ EncodedType type_
              , mbDefault : EncodedValue <$> mbDefault
              }
            )


typeAndMbDefault :: forall a. (String -> Maybe String -> a) -> P.Parser String a
typeAndMbDefault f = do
    type_ <- alphaNumToken
    _ <- Array.many P.space
    maybeDefault <- P.optionMaybe $ P.between
                  (P.char '{')
                  (P.char '}')
                  defaultValue
    pure $ f type_ maybeDefault


results :: P.Parser String (Array (Maybe (String /\ ChannelDef)))
results =
  P.choice
      [ {- Array.singleton <$>
          typeAndMbDefault
            (\type_ mbDefault -> Just $ "out" /\ ChannelDef
              { mbType : Just $ EncodedType type_
              , mbDefault : EncodedValue <$> mbDefault
              }
            )
      ,  -} Array.singleton <$> Just <$> channel
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
  pure (name /\ ChannelDef
      { mbType : EncodedType <$> maybeType
      , mbDefault : EncodedValue <$> maybeDefault
      }
  )

maybeChannel :: P.Parser String (Maybe (String /\ ChannelDef))
maybeChannel =
  P.choice
        [ P.char '?' *> pure Nothing
        , Just <$> channel
        ]


processCode :: P.Parser String ProcessCode
processCode
  =   (P.try $ Auto <$> P.between
          (P.string "/-|")
          (P.string "|-/")
          anything
      )
  <|> (P.try $ Raw <$> P.between
          (P.string "#-|")
          (P.string "|-#")
          anything
      )
  <|> (P.try $ JS <$> P.between
          (P.string "$-|")
          (P.string "|-$")
          anything
      )
  <|> (eol *> pure NoneSpecified)


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


eol :: P.Parser String Unit
eol = P.char '\n' *> pure unit