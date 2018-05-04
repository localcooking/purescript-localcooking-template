module Facebook.Types where

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Test.QuickCheck (class Arbitrary)


newtype FacebookUserId = FacebookUserId String

derive instance genericFacebookUserId :: Generic FacebookUserId
derive newtype instance eqFacebookUserId :: Eq FacebookUserId
derive newtype instance encodeJsonFacebookUserId :: EncodeJson FacebookUserId
derive newtype instance decodeJsonFacebookUserId :: DecodeJson FacebookUserId
derive newtype instance arbitraryFacebookUserId :: Arbitrary FacebookUserId
