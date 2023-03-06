module Noodle.Text.QuickDef where

import Prelude (($), (>>>), (<$>), class Show, class Eq)

import Data.Maybe (Maybe(..))


newtype FN = FN
  { tag :: String
  , family :: String
  , args :: Array (Maybe Argument)
  , returns :: String
  }

type Argument =
  { name :: String, type :: Maybe String, default :: Maybe String }


qfn :: String -> String -> Array Argument -> String -> FN
qfn tag family args = qfn' tag family $ Just <$> args

qfn' :: String -> String -> Array (Maybe Argument) -> String -> FN
qfn' tag family args returns = FN { tag, family, args, returns }

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
--   show (FN { tag, name, args, returns }) = ""


derive newtype instance Show FN
derive newtype instance Eq FN
