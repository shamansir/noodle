module Cli.Bounds where

import Prelude

import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Symbol (class IsSymbol)

import Blessed ((~<))
import Blessed.Internal.BlessedOp (BlessedOp')

import Blessed.UI.Base.Element.Property (left, top, width, height) as Element

import Cli.State (NodeBounds)

import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.BlessedSubj as K


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


-- TODO: move ::


outputPos :: NodeBounds -> { x :: Int, y :: Int }
outputPos _ = { x : 0, y : 0 }



inputPos :: NodeBounds -> { x :: Int, y :: Int }
inputPos _ = { x : 0, y : 0 }