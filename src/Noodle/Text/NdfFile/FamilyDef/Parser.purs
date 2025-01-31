module Noodle.Text.NdfFile.FamilyDef.Parser where

import Prelude

import Data.String.CodeUnits as StringCU
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.CodePoint.Unicode as CP
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Enum (fromEnum)
-- import Data.CodePoint.Unicode.Internal (UnicodeCategory)

import Data.Traversable (for)

import Parsing (Parser, runParser, ParseError, ParseState(..), getParserT, position, Position(..)) as P
import Parsing.String (char, string)  as P
import Parsing.Token (alphaNum, space) as P
import Parsing.Extra (sourceAt) as P
import Parsing.String.Basic (noneOf) as P
import Parsing.String.Extra (alphaNumToken, asArray) as P
import Parsing.Combinators (between, choice, option, optionMaybe, sepBy, try) as P
import Parsing.Combinators ((<?>))

import Noodle.Id (FamilyR, GroupR)
import Noodle.Id (unsafeFamilyR, unsafeGroupR) as Id
import Noodle.Fn.Signature (Sig, sig') as Sig
import Noodle.Text.FromCode (Source) as FC
import Noodle.Text.NdfFile.FamilyDef (FamilyDef(..), ProcessAssign(..))
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (ProcessCode(..))
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (parser) as PC
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..), ChannelDef(..), StateDef(..), emptyStateDef)


parser :: P.Parser String FamilyDef
parser = do
  source /\ pos <- P.sourceAt
  _ <- sep $ P.char ':'
  tag <- P.alphaNumToken <?> "tag"
  _ <- sep $ P.char ':'
  family <- P.alphaNumToken <?> "family"
  state /\ fnsig <- fnSignature family -- :: [<state>] <inputs> => <outputs>
  _ <- Array.many P.space
  maybeImpl <- P.optionMaybe PC.parser
  pure $ FamilyDef
    { group : Id.unsafeGroupR tag
    , state
    , fnsig
    , process : fromMaybe NoneSpecified maybeImpl
    }


fnSignature :: String -> P.Parser String (StateDef /\ Sig.Sig ChannelDef ChannelDef)
fnSignature family = do
  _ <- sep $ P.string "::"
  mbState <- P.try maybeState
  _ <- Array.many P.space
  inputs <- P.option [] channels <?> "inputs"
  _ <- sep $ P.string "=>"
  outputs <- results
  pure $
    fromMaybe emptyStateDef mbState
    /\ Sig.sig' family (Array.catMaybes inputs) (Array.catMaybes outputs)


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
    type_ <- P.alphaNumToken
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


defaultValue :: P.Parser String String
defaultValue =
  StringCU.fromCharArray <$> Array.some (P.noneOf [ '}' ]) -- (P.choice [ P.alphaNum, P.char '.', P.char ' ', P.char '_', P.char '%' ])


channel :: P.Parser String (String /\ ChannelDef)
channel = do
  name <- P.alphaNumToken
  maybeType <- P.optionMaybe $ P.char ':' *> P.alphaNumToken
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


channels :: P.Parser String (Array (Maybe (String /\ ChannelDef)))
channels =
  P.between
        (P.char '<')
        (P.char '>')
        $ P.asArray
        $ P.sepBy maybeChannel arrowSep


assignmentParser :: P.Parser String ProcessAssign
assignmentParser = do
  _ <- sep $ P.char '$'
  family <- P.alphaNumToken <?> "family"
  _ <- sep $ P.string "::"
  processCode <- PC.parser
  pure $ ProcessAssign $ Id.unsafeFamilyR family /\ processCode


-- TODO: return friendlier parsing errors
toolkitList :: String -> Either (P.ParseError) (Array FamilyDef)
toolkitList lines =
  for (String.split (String.Pattern "\n") lines) $ \s -> P.runParser s parser


toolkitList' :: String -> Array FamilyDef
toolkitList' lines =
  case toolkitList lines of
    Left _ -> []
    Right items -> items