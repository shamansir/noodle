module Rpd.API.Errors where
    -- ( RpdError
    -- , findPathByUuid

    -- ) where


import Prelude

import Rpd.Path (Path)
import Rpd.Path as Path
import Rpd.UUID (UUID)
import Rpd.UUID as UUID


data Kind
    = Global
    | Node -- Path.ToNode
    | Patch


newtype RpdError = RpdError String


-- instance showKind :: Show Kind where
--     show Global = "global"
--     show _ = "<kind>"
-- derive instance eqKind :: Eq Kind
-- instance showRpdError :: Show RpdError where
--     show (RpdError subj err) = err
-- instance eqRpdError :: Eq RpdError where
--     eq (RpdError subj err) (RpdError subj' err') =
--         subj' == subj' && err == err'


instance showRpdError :: Show RpdError where
    show (RpdError err) = show err
derive instance eqRpdError :: Eq RpdError


failedToFindUuid :: Path -> RpdError
failedToFindUuid path = RpdError $ "failed to find UUID for " <> show path


ftfu = failedToFindUuid


failedToFindSubject :: UUID -> RpdError
failedToFindSubject uuid = RpdError $ "failed to find subject for " <> show uuid


ftfs = failedToFindSubject


noNodePath :: Path -> RpdError
noNodePath path = RpdError $ "no node path found in " <> show path


nnp = noNodePath


noOutletFound :: Path.ToOutlet -> RpdError
noOutletFound path = RpdError $ "no outlet found at " <> show path

nof = noOutletFound
