module Noodle.API.Errors where
    -- ( NoodleError
    -- , findPathByUuid

    -- ) where


import Prelude

import Noodle.Path (Path)
import Noodle.Path as Path
import Noodle.UUID (UUID)
import Noodle.UUID as UUID


data Kind
    = Global
    | Node -- Path.ToNode
    | Patch


newtype NoodleError = NoodleError String


instance semigroupNoodleError :: Semigroup NoodleError where
    append (NoodleError one) (NoodleError two) = NoodleError $ one <> ". " <> two


-- instance showKind :: Show Kind where
--     show Global = "global"
--     show _ = "<kind>"
-- derive instance eqKind :: Eq Kind
-- instance showNoodleError :: Show NoodleError where
--     show (NoodleError subj err) = err
-- instance eqNoodleError :: Eq NoodleError where
--     eq (NoodleError subj err) (NoodleError subj' err') =
--         subj' == subj' && err == err'


instance showNoodleError :: Show NoodleError where
    show (NoodleError err) = show err
derive instance eqNoodleError :: Eq NoodleError


failedToFindUuid :: Path -> NoodleError
failedToFindUuid path = NoodleError $ "No item with path " <> show path <> " was found in registry"


ftfu = failedToFindUuid


failedToFindSubject :: UUID -> NoodleError
failedToFindSubject uuid = NoodleError $ "failed to find subject for " <> show uuid


ftfs = failedToFindSubject


noNodePath :: Path -> NoodleError
noNodePath path = NoodleError $ "no node path found in " <> show path


nnp = noNodePath


noOutletFound :: Path.ToOutlet -> NoodleError
noOutletFound path = NoodleError $ "no outlet found at " <> show path

nof = noOutletFound
