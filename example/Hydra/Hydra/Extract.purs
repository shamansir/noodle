module Hydra.Extract where


import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (catMaybes, concat) as Array
import Data.Int (floor)

import Hydra
import Hydra.API as Hydra

import Color as C


numV :: Value -> Maybe Number
numV (Num n) = Just n
numV _ = Nothing


numsV :: Value -> Array Number
numsV (Num n) = [ n ]
numsV (Seq xs) =  Array.catMaybes $ numV <$> xs
numsV _ = []


numOr :: Number -> Hydra -> Number
numOr def (Val val) = fromMaybe def $ numV val
numOr def _ = def


seq :: Hydra -> Array Value
seq (Val (Seq xs)) = xs
seq (Val v) = [ v ]
seq _ = []


seq' :: Hydra -> Array Number
seq' (Val v) = numsV v
seq' _ = []


entity :: Hydra -> Maybe Texture
entity = toTexture


modifier :: Hydra -> Maybe Modifier
modifier = toModifier


colorMod :: Hydra -> Array C.Color
colorMod =
    case _ of
        (Tex (Texture _ modifiers)) -> fromModifiers modifiers
        _ -> []
    where
        fromModifiers :: Array Modifier -> Array C.Color
        fromModifiers modifiers = Array.concat $ Array.catMaybes $ extractColors <$> modifiers
        extractColors :: Modifier -> Maybe (Array C.Color)
        extractColors (C (Color {r, g, b, a})) = Just $ joinToColor <$> numsV r <*> numsV g <*> numsV b <*> numsV a
        extractColors _ = Nothing
        joinToColor r g b a = C.rgba (floor $ r * 255.0) (floor $ g * 255.0) (floor $ b * 255.0) a


buildSeq5 :: Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Hydra
buildSeq5 h1 h2 h3 h4 h5 =
    Val $ Seq $ seqOf h1 <> seqOf h2 <> seqOf h3 <> seqOf h4 <> seqOf h5
    where seqOf maybeH = fromMaybe [] (seq <$> maybeH)


performMaybe :: Maybe Hydra -> Maybe Hydra -> Maybe Hydra -> Maybe Hydra
performMaybe (Just (Val valA)) (Just (Op op)) (Just (Val valB)) = Just $ Val $ Hydra.expr op valA valB
performMaybe _                 _              _                 = Nothing