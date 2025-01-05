module Cli.HasEditors where

import Prelude


import Prelude

import Effect (Effect)

import Data.Map (Map)
import Data.Maybe (Maybe)

import Noodle.Node as Node


{-
newtype EditorId = EditorId String


derive instance Eq EditorId
derive instance Ord EditorId


type Editors repr = Map EditorId (Maybe (repr -> Effect Unit))


type HasEditors repr r = { editors :: Editors repr | r }
-}