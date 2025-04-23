module StarterTk.Patch where

import Prelude


data PState = PState


init :: forall m. Applicative m => m PState
init = pure PState