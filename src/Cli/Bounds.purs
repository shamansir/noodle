module Cli.Bounds where

import Prelude

import Effect.Exception (Error)

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (get) as State

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))

import Blessed ((~<))
import Blessed.Internal.BlessedOp (BlessedOp', BlessedOpGet)

import Blessed.UI.Base.Element.Property (left, top, width, height) as Element

import Cli.State (NodeBounds, State)

import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.BlessedSubj as K

import Noodle.Id as Id


collect
    :: forall (state :: Type) (m :: Type -> Type) (subj :: K.Subject) (key :: Symbol)
    .  K.Extends K.Element subj
    => K.IsSubject subj
    => IsSymbol key
    => MonadThrow Error m
    => NodeKey subj key → BlessedOp' state m NodeBounds
collect node = do
    left <- Element.left ~< node
    top <- Element.top ~< node
    width <- Element.width ~< node
    height <- Element.height ~< node
    pure { top, left, width, height }


loadOrCollect
    :: forall subj key m
    .  K.Extends K.Element subj
    => K.IsSubject subj
    => IsSymbol key
    => MonadThrow Error m
    => Id.NodeIdR
    -> NodeKey subj key
    -> BlessedOpGet State m NodeBounds
loadOrCollect nodeId nodeKey = do
    locations <- _.locations <$> State.get
    case Map.lookup nodeId locations of
        Just nodeBounds -> pure nodeBounds
        Nothing -> collect nodeKey -- FIXME: since we update bounds every time in the state, there's no need to collect them


outputPos :: NodeBounds -> Int -> { x :: Int, y :: Int }
outputPos n outputIdx =
    { x : n.left + (outputIdx * 4)
    , y : n.top + 4
    }



inputPos :: NodeBounds -> Int -> { x :: Int, y :: Int }
inputPos n inputIdx =
    { x : n.left + (inputIdx * 4)
    , y : n.top
    }


move :: NodeBounds -> NodeBounds -> NodeBounds
move newBounds =
    _ { top = newBounds.top, left = newBounds.left }