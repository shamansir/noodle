module Cli.Bounds where

import Prelude

import Effect.Exception (Error)
import Effect.Console (log) as Console
import Effect.Class (liftEffect, class MonadEffect)

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (get) as State

import Data.Symbol (class IsSymbol)
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))

import Blessed ((~<))
import Blessed.Internal.BlessedOp (BlessedOp', BlessedOpGet)

import Blessed.UI.Base.Element.Property (aleft, atop, width, height) as Element

import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.BlessedSubj as K

import Noodle.Id as Id


type Bounds =
    { top :: Int
    , left :: Int
    , width :: Int
    , height :: Int
    }


type NodeBounds = Bounds


collect
    :: forall (state :: Type) (m :: Type -> Type) (subj :: K.Subject) (key :: Symbol)
    .  K.Extends K.Element subj
    => K.IsSubject subj
    => IsSymbol key
    => MonadThrow Error m
    => Id.NodeR
    -> NodeKey subj key
    -> BlessedOp' state m Bounds
collect _ node = do
    left   <- Element.aleft ~< node
    top    <- Element.atop ~< node
    width  <- Element.width ~< node
    height <- Element.height ~< node
    pure { top, left, width, height }


{- REM
loadOrCollect
    :: forall subj key mo tk s fs r mi
    .  K.Extends K.Element subj
    => K.IsSubject subj
    -- => MonadEffect m
    => IsSymbol key
    => MonadThrow Error mo
    => Id.NodeR
    -> NodeKey subj key
    -> BlessedOpGet (State tk s fs r mi) mo NodeBounds
loadOrCollect nodeId nodeKey = do
    locations <- _.locations <$> State.get
    -- liftEffect $ Console.log $ ( show $ Map.lookup nodeId locations )
    case Map.lookup nodeId locations of
        Just nodeBounds -> pure nodeBounds
        Nothing -> collect nodeId nodeKey
            -- FIXME: pure { top : 0, left : 0, width : 0, height : 0 },
            -- since we update bounds every time in the state, there's no need to collect them
-}


outletPos :: NodeBounds -> Int -> { x :: Int, y :: Int }
outletPos n outletIdx =
    -- { x : n.left + 1 + (outletIdx * 4)
    -- , y : n.top + 5
    -- }
    { x : n.left + (outletIdx * 4)
    , y : n.top + (n.height - 1)
    }



inletPos :: NodeBounds -> Int -> { x :: Int, y :: Int }
inletPos n inletIdx =
    -- { x : n.left + 1 + (inletIdx * 4)
    -- , y : n.top + 1
    -- }
    { x : n.left + (inletIdx * 4)
    , y : n.top - 2
    }


move :: NodeBounds -> NodeBounds -> NodeBounds
move newBounds =
    _ { top = newBounds.top, left = newBounds.left }