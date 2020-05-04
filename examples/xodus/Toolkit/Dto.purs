module Xodus.Toolkit.Dto where

import Data.Newtype (class Newtype, unwrap)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Encode as J
import Data.Argonaut.Decode as J


newtype Database =
    Database
        { uuid :: String
        , location :: String
        , encrypted :: Boolean
        , opened :: Boolean
        }


-- [{"uuid":"49f3ca7e-7985-4e09-9389-1cb6d5c2e762","location":"localhost:3666","key":null,"encryptionProvider":null,"encryptionKey":null,"encryptionIV":null,"encrypted":false,"opened":true},{"uuid":"a5ce2ed2-7119-41c0-a2ad-a430280b8724","location":"noodle","key":null,"encryptionProvider":null,"encryptionKey":null,"encryptionIV":null,"encrypted":false,"opened":true}]



derive instance newtypeAppUser :: Newtype Database _

derive newtype instance encodeJsonAppUser :: J.EncodeJson Database
derive newtype instance decodeJsonAppUser :: J.DecodeJson Database
