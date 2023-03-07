module Noodle.Text.QuickDef where

import Prelude (($), (>>>), (<$>), class Show, class Eq)

import Data.Maybe (Maybe(..))


newtype QFamily = QFamily
  { tag :: String
  , family :: String
  , inputs :: Array (Maybe Channel)
  , outputs :: Array (Maybe Channel)
  }

type Channel =
  { name :: String, type :: Maybe String, default :: Maybe String }


qfm :: String -> String -> Array Channel -> String -> QFamily
qfm tag family inputs = qfm' tag family $ Just <$> inputs

qfm' :: String -> String -> Array (Maybe Channel) -> String -> QFamily
qfm' tag family inputs returns = QFamily { tag, family, inputs, outputs : [ qout returns ] }

qfmo :: String -> String -> Array Channel -> Array Channel -> QFamily
qfmo tag family inputs outputs = qfmo' tag family (Just <$> inputs) (Just <$> outputs)

qfmo' :: String -> String -> Array (Maybe Channel) -> Array (Maybe Channel) -> QFamily
qfmo' tag family inputs outputs = QFamily { tag, family, inputs, outputs }

qchan :: String -> Channel
qchan name = { name, type : Nothing, default : Nothing }

qchand :: String -> String -> Channel
qchand name def = { name, type : Nothing, default : Just def }

qchan' :: String -> Maybe Channel
qchan' = qchan >>> Just

qchant :: String -> String -> Channel
qchant name t = { name, type : Just t, default : Nothing }

qchant' :: String -> String -> Maybe Channel
qchant' n = qchant n >>> Just

qchantd :: String -> String -> String -> Channel
qchantd name t d = { name, type : Just t, default : Just d }

qchantd' :: String -> String -> String -> Maybe Channel
qchantd' n t = qchantd n t >>> Just

qout :: String -> Maybe Channel
qout type_ = Just { name : "out", type : Just type_, default : Nothing }

-- instance Show QFamily where
--   show (QFamily { tag, name, args, returns }) = ""


derive newtype instance Show QFamily
derive newtype instance Eq QFamily
