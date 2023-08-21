module Cli.Tagging where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Mark (mark)
import Data.SProxy (reflect)


import Noodle.Id as Id


import Blessed.Tagger (Tag)
import Blessed.Tagger as T

import Cli.Palette as Palette
import Cli.Palette.Item (crepr) as C
import Cli.Palette.Item (Item, fullInfo) as Palette
import Cli.Palette.Set.X11 as X11
import Cli.Palette.Set.Pico8 as Pico



import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra
import Toolkit.Hydra2.Repr.Info (short, full) as Info
import Toolkit.Hydra2.Group (toGroup, toGroupR) as Hydra



input :: forall i. IsSymbol i => Int -> Id.Input i -> Maybe Hydra.WrapRepr -> Tag
input idx inputId = input' idx $ Id.inputR inputId


input' :: Int -> Id.InputR -> Maybe Hydra.WrapRepr -> Tag
input' idx inputId (Just repr) =
    T.fgcs (mark repr) $ Info.short repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
input' idx inputId Nothing = T.s "⋱" <> (T.s $ show idx) <> T.s "⋰"


inputInfoBox :: forall i. IsSymbol i => Id.Input i -> Tag
inputInfoBox inputId =
    T.fgcs (C.crepr Palette.inputId) $ reflect inputId


inputStatusLine :: forall f i. IsSymbol i => IsSymbol f => Id.Family' f -> Int -> Id.Input i -> Maybe Hydra.WrapRepr -> Tag
inputStatusLine family idx inputId = inputStatusLine' (Id.familyR' family) idx $ Id.inputR inputId


inputStatusLine' :: Id.FamilyR -> Int -> Id.InputR -> Maybe Hydra.WrapRepr -> Tag
inputStatusLine' familyR idx inputId (Just repr) =
    -- TODO: show node id and group as well
    (T.fgcs (C.crepr Palette.familyName) $ Id.reflectFamilyR familyR) <> T.s " " <> (T.fgcs (C.crepr Palette.inputId) $ Id.reflectInputR inputId) <> T.s " " <> (T.fgcs (mark repr) $ Info.full repr) -- "⋱" <> show idx <> "⋰" <> Info.short repr
inputStatusLine' familyR idx inputId Nothing =
    T.s "⋱" <> (T.s $ show idx) <> T.s "⋰"


output ::forall o. IsSymbol o => Int -> Id.Output o -> Maybe Hydra.WrapRepr -> Tag
output idx outputId = output' idx $ Id.outputR outputId


output' :: Int -> Id.OutputR -> Maybe Hydra.WrapRepr -> Tag
output' idx outputId (Just repr) =
    T.fgcs (mark repr) $ Info.short repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
    -- Info.short repr -- "⋰" <> show idx <> "⋱" <> Info.short repr
output' idx outputId Nothing = T.s "⋰" <> (T.s $ show idx) <> T.s "⋱"


outputInfoBox :: forall o. IsSymbol o => Id.Output o -> Tag
outputInfoBox outputId =
    T.fgcs (C.crepr Palette.outputId) $ reflect outputId


outputStatusLine ::forall f o. IsSymbol f => IsSymbol o => Id.Family' f -> Int -> Id.Output o -> Maybe Hydra.WrapRepr -> Tag
outputStatusLine family idx outputId = outputStatusLine' (Id.familyR' family) idx $ Id.outputR outputId


outputStatusLine' :: Id.FamilyR -> Int -> Id.OutputR -> Maybe Hydra.WrapRepr -> Tag
outputStatusLine' familyR idx outputId (Just repr) =
    -- TODO: show group as well
    (T.fgcs (C.crepr Palette.familyName) $ Id.reflectFamilyR familyR) <> T.s " " <> (T.fgcs (C.crepr Palette.outputId) $ Id.reflectOutputR outputId) <> T.s " " <> (T.fgcs (mark repr) $ Info.full repr) -- "⋱" <> show idx <> "⋰" <> Info.short repr
    --T.fgcs (mark repr) $ Info.full repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
    -- Info.short repr -- "⋰" <> show idx <> "⋱" <> Info.short repr
outputStatusLine' familyR idx outputId Nothing = T.s "⋰" <> (T.s $ show idx) <> T.s "⋱"


nodeLabel :: forall f. IsSymbol f => Id.Family f -> Tag
nodeLabel family =
    let color = mark $ Hydra.toGroup family
    in T.bgc (C.crepr Palette.nodeBg) $ T.fgc color $ T.s $ Id.reflectFamily family


nodeMouseOver :: forall f. IsSymbol f => Id.Family f -> Tag
nodeMouseOver family =
    T.fgcs (C.crepr Palette.familyName) $ reflect family


removeButtonOut ∷ Tag
removeButtonOut =
    T.fgcs (C.crepr Pico.blue) "⨯"


removeButtonOver ∷ Tag
removeButtonOver =
    T.fgcs (C.crepr Pico.red) "⨯" -- "╳"


removeInfoBox ∷ Tag
removeInfoBox =
    T.fgcs (C.crepr Pico.red) "remove"


removeStatusLine :: forall f. IsSymbol f => Id.Family f -> Tag
removeStatusLine family =
    T.fgcs (C.crepr Pico.red) "remove" <> T.s " " <> (T.fgcs (C.crepr Palette.familyName) $ reflect family)


libraryItem :: Id.FamilyR -> Tag
libraryItem familyR =
    let color = mark $ Hydra.toGroupR familyR
    in T.fgc color $ T.s $ Id.reflectFamilyR familyR


paletteItem :: Palette.Item -> Tag
paletteItem item =
    T.bg item.repr (T.s "      ") <> T.s " " <> T.fg item.repr (T.s $ Palette.fullInfo item)


-- Commands

toolkit :: String -> Tag
toolkit = T.fgc (C.crepr Palette.toolkit) <<< T.s


version :: Number -> Tag
version = T.fgc (C.crepr Palette.version) <<< T.s <<< show


family :: String -> Tag
family = T.fgc (C.crepr Palette.familyName) <<< T.s


nodeId :: String -> Tag
nodeId = T.fgc (C.crepr Palette.nodeId) <<< T.s


operator :: String -> Tag
operator = T.fgc (C.crepr Palette.operator) <<< T.s


value :: String -> Tag
value = T.fgc (C.crepr Palette.value) <<< T.s


coord :: Int -> Tag
coord = T.fgc (C.crepr Palette.coord) <<< T.s <<< show


inputIdx :: Int -> Tag
inputIdx = T.fgc (C.crepr Palette.inputIdx) <<< T.s <<< show


outputIdx :: Int -> Tag
outputIdx = T.fgc (C.crepr Palette.outputIdx) <<< T.s <<< show


inputId :: String -> Tag
inputId = T.fgc (C.crepr Palette.inputId) <<< T.s


outputId :: String -> Tag
outputId = T.fgc (C.crepr Palette.outputId) <<< T.s


buttonToggle ::String -> Boolean -> Tag
buttonToggle repr true = T.fgc (C.crepr Palette.positive) $ T.s repr
buttonToggle repr false = T.fgc (C.crepr Palette.neutral) $ T.s repr


buttonConnection :: Either String Int -> Tag
buttonConnection (Left s) = T.fgc (C.crepr Palette.negative) $ T.s s
buttonConnection (Right 0) = T.fgc (C.crepr Palette.neutral) $ T.s "0"
buttonConnection (Right n) = T.fgc (C.crepr Palette.positive) $ T.s $ show n


outputHover :: Tag
outputHover = T.fgc (C.crepr Palette.neutral) $ T.s "-"


outputSelect :: Tag
outputSelect = T.fgc (C.crepr Palette.positive) $ T.s "~"


inputHover :: Tag
inputHover = T.fgc (C.crepr Palette.neutral) $ T.s "-"


infoNode :: Hydra.WrapRepr -> Tag
infoNode repr =
    T.fgcs (mark repr) $ Info.full repr


selected :: String -> Tag
selected = T.fgc (C.crepr Palette.positive) <<< T.s