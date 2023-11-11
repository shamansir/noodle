module Prev.Web.App.Component.PatchTabs where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action = AddPatch

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> AddPatch ] []
      ]

  handleAction = case _ of
    AddPatch ->
      H.modify_ \state -> state - 1
