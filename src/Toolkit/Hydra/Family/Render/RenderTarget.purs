module Toolkit.Hydra.Family.Render.RenderTarget where

import Prelude


data RenderTarget

data RenderItem


-- data Renderer

data FF


foreign import data Test :: Symbol -> FF


foreign import data Cli :: RenderTarget
foreign import data Web :: RenderTarget


-- foreign import data R :: Renderer


-- data RXD :: Renderer
-- data RXD = RD


data RendersFamily :: RenderTarget -> Symbol -> Type
data RendersFamily rt f = RendersFamily


data RendersTo :: RenderTarget -> Type
data RendersTo rt = RendersTo
