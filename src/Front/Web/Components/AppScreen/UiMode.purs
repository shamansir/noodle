module Web.Components.AppScreen.UiMode where

import Prelude

data UiMode
    = CanvasFullyVisible
    | TransparentOverlay Number -- semi-transparent overlay over canvas
    | SolidOverlay UiMode -- solid color over the canvas (canvas not visible), keeps previous mode to get back to it
    | OnlyCanvas UiMode -- UI is hidden, keeps previous mode to get back to it


data UiModeKey
    = KCanvasFullyVisible
    | KTransparentOverlay -- semi-transparent overlay over canvas
    | KSolidOverlay-- solid color over the canvas (canvas not visible)
    | KOnlyCanvas -- UI is hidden


getModeKey :: UiMode -> UiModeKey
getModeKey = case _ of
    CanvasFullyVisible -> KCanvasFullyVisible
    TransparentOverlay _ -> KTransparentOverlay
    SolidOverlay _ -> KSolidOverlay
    OnlyCanvas _ -> KOnlyCanvas


allModes = [ KCanvasFullyVisible, KTransparentOverlay, KSolidOverlay, KOnlyCanvas ] :: Array UiModeKey


canvasChar :: UiModeKey -> String
canvasChar = case _ of
    KCanvasFullyVisible -> "●"
    KTransparentOverlay -> "●" -- "⊜" -- "○"
    KSolidOverlay -> "◌" -- "⊘" -- "◌"
    KOnlyCanvas -> "●"


overlayChar :: UiModeKey -> String
overlayChar = case _ of
    KCanvasFullyVisible -> "◌"
    KTransparentOverlay -> "⊜"
    KSolidOverlay -> "●"
    KOnlyCanvas -> "◌"


nodesChar :: UiModeKey -> String
nodesChar = case _ of
    KCanvasFullyVisible -> "●"
    KTransparentOverlay -> "●"
    KSolidOverlay -> "●"
    KOnlyCanvas -> "◌"


modeString :: UiModeKey -> String
modeString mk = nodesChar mk <> overlayChar mk <> canvasChar mk


nextKey :: UiModeKey -> UiModeKey
nextKey = case _ of
    KOnlyCanvas -> KTransparentOverlay
    KTransparentOverlay -> KSolidOverlay
    KSolidOverlay -> KCanvasFullyVisible
    KCanvasFullyVisible -> KOnlyCanvas

nextMode :: UiMode -> UiModeKey -> UiMode
nextMode curMode = case _ of
    KOnlyCanvas -> OnlyCanvas curMode
    KTransparentOverlay -> TransparentOverlay 0.5 -- FIXME:
    KSolidOverlay -> SolidOverlay curMode
    KCanvasFullyVisible -> CanvasFullyVisible
