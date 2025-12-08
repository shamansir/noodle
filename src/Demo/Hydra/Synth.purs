module HydraTk.Synth
    ( perform
    , SynthAction(..), SharedAction(..), ScenePreviewAction(..), NodeBodiesAction(..)
    , startSynth, resizeSynth, executeHydraCode, drawNodeBody, clearNodeBodies
    , HydraScene(..)
    , class ToHydraScene, toHydraScene
    ) where

import Prelude

import Effect (Effect)
import Noodle.Id (NodeR) as Id

import Data.Maybe (Maybe(..))

import Foreign (Foreign)

import Front.Shared.Bounds (Bounds)


data SynthAction
    = WithBoth SharedAction
    | WithPreview ScenePreviewAction
    | WithNodeBodies NodeBodiesAction


data SharedAction
    = StartSynth
    | ResizeSynth Int Int


data ScenePreviewAction
    = ExecuteHydraCode String


data NodeBodiesAction
    = DrawNodeBody Id.NodeR { node :: Bounds, body :: Bounds } HydraScene
    -- | RedrawAllNodeBodies (Array (Id.NodeR /\ { node :: Bounds, body :: Bounds })) -- TODO
    | ClearNodeBodies


perform :: SynthAction -> Effect Unit
perform = case _ of
    WithBoth StartSynth -> h_runHydra
    WithBoth (ResizeSynth w h) -> h_resize w h
    WithPreview (ExecuteHydraCode code) -> h_executeHydra code
    WithNodeBodies (DrawNodeBody nodeR bounds scene) -> h_drawNodeSceneOf nodeR bounds scene
    WithNodeBodies ClearNodeBodies -> h_clearNodeScenes


newtype HydraScene = HydraScene Foreign


class ToHydraScene a where
    toHydraScene :: a -> HydraScene


startSynth = WithBoth StartSynth :: SynthAction
resizeSynth w h = (WithBoth $ ResizeSynth w h) :: SynthAction
executeHydraCode code = (WithPreview $ ExecuteHydraCode code) :: SynthAction
drawNodeBody nodeR bounds scene = (WithNodeBodies $ DrawNodeBody nodeR bounds scene) :: SynthAction
clearNodeBodies = (WithNodeBodies ClearNodeBodies) :: SynthAction


foreign import h_runHydra :: Effect Unit
foreign import h_resize :: Int -> Int -> Effect Unit
foreign import h_executeHydra :: String -> Effect Unit
foreign import h_drawNodeSceneOf :: Id.NodeR -> { node :: Bounds, body :: Bounds } -> HydraScene -> Effect Unit
foreign import h_clearNodeScenes :: Effect Unit