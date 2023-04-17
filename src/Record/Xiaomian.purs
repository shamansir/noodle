module Record.Xiaomian where

{- from https://raw.githubusercontent.com/justinwoo/purescript-xiaomian/master/src/Xiaomian.purs -}

import Record.Naporitan as N
import Prim.Row as Row
import Prim.RowList as RL
import Type.Prelude (RProxy(..), SProxy)

getKeysRow :: forall proxy r keys. GetKeysRow r keys => proxy r -> RProxy keys
getKeysRow _ = RProxy

getKeysRecord
  :: forall proxy r keys
   . GetKeysRow r keys
  => N.ReflectRecordProxy { | keys }
  => proxy r
  -> { | keys }
getKeysRecord _ = N.reflectRecordProxy

getKeysRecord'
  :: forall proxy rproxy r keys
   . GetKeysRow r keys
  => N.ReflectRecordProxy { | keys }
  => proxy (rproxy r)
  -> { | keys }
getKeysRecord' _ = N.reflectRecordProxy

class GetKeysRow (r :: # Type) (keys :: # Type) | r -> keys

instance getKeysRowInst ::
  ( RL.RowToList r rl
  , GetKeysRowInst rl keys
  ) => GetKeysRow r keys

class GetKeysRowInst (rl :: RL.RowList Type) (keys :: # Type)
  | rl -> keys

instance getKeysRowInstNil :: GetKeysRowInst RL.Nil ()

instance getKeysRowInstCons ::
  ( GetKeysRowInst tail keys'
  , Row.Cons name (SProxy name) keys' keys
  ) => GetKeysRowInst (RL.Cons name ty tail) keys