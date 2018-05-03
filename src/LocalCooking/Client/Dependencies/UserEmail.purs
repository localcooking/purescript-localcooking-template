module LocalCooking.Client.Dependencies.UserEmail where

import LocalCooking.Client.Dependencies.AccessToken.Generic (AuthInitIn, AuthInitOut)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Generic (class Generic, gShow)
import Text.Email.Validate (EmailAddress, emailAddress)
import Control.Alternative ((<|>))
import Sparrow.Client.Queue (SparrowStaticClientQueues)


type UserEmailInitIn = AuthInitIn AuthToken JSONUnit

type UserEmailInitOut = AuthInitOut EmailAddress


type UserEmailSparrowClientQueues eff =
  SparrowStaticClientQueues eff UserEmailInitIn UserEmailInitOut
