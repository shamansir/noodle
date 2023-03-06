module Noodle.Text.QuickDef where

import Prelude (($), (>>>), (<$>), class Show, class Eq)

import Data.Maybe (Maybe(..))


newtype FN = FN
  { family :: String
  , name :: String
  , args :: Array (Maybe Argument)
  , returns :: String
  }

type Argument =
  { name :: String, type :: Maybe String, default :: Maybe String }


qfn :: String -> String -> Array Argument -> String -> FN
qfn family name args = qfn' family name $ Just <$> args

qfn' :: String -> String -> Array (Maybe Argument) -> String -> FN
qfn' family name args returns = FN { family, name, args, returns }

qarg :: String -> Argument
qarg name = { name, type : Nothing, default : Nothing }

qarg' :: String -> Maybe Argument
qarg' = qarg >>> Just

qargt :: String -> String -> Argument
qargt name t = { name, type : Just t, default : Nothing }

qargt' :: String -> String -> Maybe Argument
qargt' n = qargt n >>> Just

qargtd :: String -> String -> String -> Argument
qargtd name t d = { name, type : Just t, default : Just d }

qargtd' :: String -> String -> String -> Maybe Argument
qargtd' n t = qargtd n t >>> Just


-- instance Show FN where
--   show (FN { family, name, args, returns }) = ""


derive newtype instance Show FN
derive newtype instance Eq FN
