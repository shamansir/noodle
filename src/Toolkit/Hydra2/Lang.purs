module Toolkit.Hydra2.Lang where

import Prelude

import Prelude (Unit, unit, discard, (#), pure, bind, ($), (<>))
import Prelude (class Show, show) as Core

import Effect (Effect)
import Effect.Console as Console

import Data.Functor (class Functor)
import Data.Semigroup (class Semigroup)
import Control.Apply (class Apply)
import Control.Bind (class Bind)


import Toolkit.Hydra2.Types
import Toolkit.Hydra2.Types (Value(..)) as T

import Toolkit.Hydra2.Lang.ToCode
import Toolkit.Hydra2.Lang.ToCode (fn) as ToCode


type Chainable = Texture


{-
data Chainable -- TODO / FIXME `Types.Texture` is almost the same
    = WithColor ColorOp Chainable
    | ModulateWith Chainable Modulate Chainable
    | Geometry Geometry Chainable
    | BlendOf Chainable Blend Chainable
    | From Source
    | None
-}


data Single
    = WithAudio String -- FIXME
    | InitCam Source
    | Render Output


data Command
    = Unknown
    | End Output Chainable
    | Pair Command Command -- parent -> child ?
    -- | Batch (Array Command)
    | One Single
    | Continue Chainable
    | To From


data Program a = -- same as Writer?
    Program Command a


unknown ∷ Program Unit -- private
unknown = Program Unknown unit


q :: Command -> Program Unit -- private
q cmd = Program cmd unit


instance Semigroup Command where
    append = Pair


-- instance Semigroup (Program Unit) where
--     append :: Program Unit -> Program Unit -> Program Unit
--     append (Program cmdA _) (Program cmdB _) =
--         Program ( cmdA <> cmdB ) unit


instance Semigroup a => Semigroup (Program a) where
    append :: Program a -> Program a -> Program a
    append (Program cmdA a) (Program cmdB b) =
        Program ( cmdA <> cmdB ) (a <> b)


instance Functor Program where
    map :: forall a b. (a -> b) -> Program a -> Program b
    map fa (Program aitems a) =
        Program aitems $ fa a


instance Apply Program where
    apply :: forall a b. Program (a -> b) -> Program a -> Program b
    apply (Program fitems fa) (Program aitems a) =
        Program (fitems <> aitems) $ fa a


instance Bind Program where
    bind :: forall a b. Program a -> (a -> Program b) -> Program b
    bind (Program aitems aval) f =
        case f aval of
            Program bitems b -> Program (aitems <> bitems) b


instance Core.Show Command where
    show Unknown = "unknown"
    show (End _ chainable) = "end { " <> Core.show chainable <> " }"
    show (Pair cmdA cmdB) = Core.show cmdA <> " -> " <> Core.show cmdB
    show (One (WithAudio str)) = "1 with audio"
    show (One (InitCam str)) = "1 init cam"
    show (One (Render _)) = "1 render"
    show (Continue chainable) = "cont with ( " <> Core.show chainable <> " )"
    show (To _) = "out"
    show _ = "???"


instance ToCode Command where
    toCode = case _ of
        Unknown -> "/* unknown */"
        End output chainable -> toCode chainable <> "." <> ToCode.fn "out" [ output ]
        Pair cmdA cmdB -> toCode cmdA <> "\n" <> toCode cmdB
        One (WithAudio str) -> "withAudio()"
        One (InitCam str) -> "withCam()"
        One (Render _) -> "render()"
        Continue chainable -> "." <> toCode chainable
        To _ -> "to()"
        -- _ -> "???"


instance ToCode (Program a) where
    toCode (Program cmd _) = toCode cmd


{-
instance Core.Show Chainable where
    show (WithColor _ c) = "color <- " <> Core.show c
    show (ModulateWith ca _ cb) = "mod <- " <> Core.show ca <> " <- " <> Core.show cb
    show (Geometry _ c) = "geom <- " <> Core.show c
    show (BlendOf ca _ cb) = "blend <- " <> Core.show ca <> " <- " <> Core.show cb
    show (From _) = "src"
    show None = "none"
-}


{-}
instance Core.Show Command where
    show Unknown = "unknown"
    show End = "end"
    show (Pair cmdA cmdB) = Core.show cmdA <> " -> " <> Core.show cmdB
    show (One (WithFrom from)) = "with from"
    show (One (WithAudio str)) = "with audio"
    show (One (InitCam str)) = "init cam"
    show (One (Render _)) = "render"
    show (Continue (WithColor _)) = "cont with color"
    show (Continue (WithModulate _)) = "cont with mod"
    show (Continue (WithGeometry _)) = "cont with geom"
    show (Continue (WithSource _)) = "cont with src"
    show (To from) = "out"
-}

instance Core.Show a => Core.Show (Program a) where
    show :: Program a -> String
    show (Program items a) = Core.show items <> "    " <> Core.show a


commandOf :: forall a. Program a -> Command
commandOf (Program cmd _) = cmd


s0 ∷ Source
s0 = S0


o0 ∷ Output
o0 = Output0


initCam = q <<< One <<< InitCam


setBins _ _ = unknown


src :: Source -> Chainable
src = From


a = unknown

show _ = unknown


osc ∷ Value → Value → Value → Chainable
osc frequency sync offset =
    From $ Osc { frequency, sync, offset }


modulate :: Chainable -> Value -> Chainable -> Chainable
modulate what value with =
    ModulateWith { what, with } $ Modulate value


saturate :: Value -> Chainable -> Chainable
saturate v =
    flip WithColor $ Saturate v


pixelate ∷ Value → Value → Chainable -> Chainable
pixelate pixelX pixelY =
    flip Geometry $ GPixelate { pixelX, pixelY }


scale ∷ Value → Chainable → Chainable
scale amount =
    flip Geometry $ GScale
        { amount
        , offsetX : Number 0.0
        , offsetY : Number 0.0
        , xMult : Number 1.0
        , yMult : Number 1.0
        }


fn _ = T.None


out :: Output -> Chainable -> Program Unit
out output =
    q <<< End output


fft _ _ _ = unknown


render :: Output -> Program Unit
render = q <<< One <<< Render


n :: Number -> Value
n = Number