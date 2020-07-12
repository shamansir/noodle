module Noodle.Render.DebugBox where

import Prelude

import Data.Either (Either(..))
import Data.List (List, (:))
import Data.List as List
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.API.Action (Action(..)) as Core
import Noodle.Network as R


actionStackSize :: Int
actionStackSize = 10


data Action
    = Invert ActionsKind


data ActionsKind
    = Build
    | Data
    | Inner
    | Request


instance showActionsKind :: Show ActionsKind where
    show Build = "Build"
    show Inner = "Inner"
    show Data = "Data"
    show Request = "Request"


derive instance eqActionsKind :: Eq ActionsKind


data FilterState = On | Off


type Filter = List (ActionsKind /\ FilterState)


type Model d c n =
    { lastActions :: List (Core.Action d c n)
    , filter :: Filter
    }


init :: forall d c n. Model d c n
init =
    { lastActions : List.Nil
    , filter
        : (Build /\ On)
        : (Data /\ Off)
        : (Request /\ Off)
        : (Inner /\ Off)
        : List.Nil
    }


update
    :: forall d c n
     . Either Action (Core.Action d c n)
    -> R.Network d c n /\ Model d c n
    -> Model d c n
update (Right action) (nw /\ model) =
    model
        { lastActions =
            if (not $ filtered)
            then
                action :
                    (if List.length model.lastActions < actionStackSize then
                        model.lastActions
                    else
                        List.take actionStackSize model.lastActions
                    )
            else model.lastActions
        }
    where
        filtered = List.foldr check true model.filter
        check (kind /\ state) prev =
            prev && case action /\ kind /\ state of
                Core.Build _   /\ Build   /\ On -> false
                Core.Data _    /\ Data    /\ On -> false
                Core.Inner _   /\ Inner   /\ On -> false
                Core.Request _ /\ Request /\ On -> false
                _ -> true

update (Left (Invert kind)) (nw /\ model) =
    model
        { filter = switchIfKind <$> model.filter
        }
    where
        switchIfKind (otherKind /\ On)  | otherKind == kind = otherKind /\ Off
        switchIfKind (otherKind /\ Off) | otherKind == kind = otherKind /\ On
        switchIfKind (otherKind /\ val) | otherwise         = otherKind /\ val
