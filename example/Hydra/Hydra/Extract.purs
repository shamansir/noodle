module Hydra.Extract where


import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (catMaybes, concat) as Array
import Data.Int (floor)

import Hydra

import Halogen.Svg.Attributes (Color(..)) as S


numV :: Value -> Maybe Number
numV (Num n) = Just n
numV _ = Nothing


numsV :: Value -> Array Number
numsV (Num n) = [ n ]
numsV (Seq xs) =  Array.catMaybes $ numV <$> xs
numsV _ = []


numOr :: Number -> Hydra -> Number
numOr def (Value val) = fromMaybe def $ numV val
numOr def _ = def


seq :: Hydra -> Array Value
seq (Value (Seq xs)) = xs
seq (Value v) = [ v ]
seq _ = []


seq' :: Hydra -> Array Number
seq' (Value v) = numsV v
seq' _ = []


colorMod :: Hydra -> Array S.Color
colorMod (Hydra (Entity _ modifiers)) =
    Array.concat $ Array.catMaybes $ extractColors <$> modifiers
    where
        extractColors :: Modifier -> Maybe (Array S.Color)
        extractColors (C (Color {r, g, b, a})) = Just $ joinToColor <$> numsV r <*> numsV g <*> numsV b <*> numsV a
        extractColors _ = Nothing
        joinToColor r g b a = S.RGBA (floor $ r * 255.0) (floor $ g * 255.0) (floor $ b * 255.0) a
colorMod _ = []


buildSeq5 :: Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Hydra
buildSeq5 h1 h2 h3 h4 h5 =
    Value $ Seq $ seqOf h1 <> seqOf h2 <> seqOf h3 <> seqOf h4 <> seqOf h5
    where seqOf maybeH = fromMaybe [] (seq <$> maybeH)
