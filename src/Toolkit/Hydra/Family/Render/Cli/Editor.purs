module Toolkit.Hydra.Family.Render.Editor where

import Prelude

import Effect (Effect)
import Toolkit.Hydra.Repr.Wrap (WrapRepr) as Hydra


import Data.Map (Map)
import Data.Maybe (Maybe)

import Noodle.Node as Node


newtype EditorId = EditorId String


derive instance Eq EditorId
derive instance Ord EditorId


type Editors = Map EditorId (Maybe (Hydra.WrapRepr -> Effect Unit))


type HasEditors r = { editors :: Editors | r }