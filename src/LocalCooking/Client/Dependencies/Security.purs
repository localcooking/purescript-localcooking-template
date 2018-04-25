module LocalCooking.Client.Dependencies.Security where

import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Common.Password (HashedPassword)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Data.Generic (class Generic, gShow)
import Text.Email.Validate (EmailAddress)
import Text.Email.Validate as Email
import Control.Alternative ((<|>))
import Sparrow.Client.Queue (SparrowStaticClientQueues)



newtype SecurityInitIn = SecurityInitIn
  { authToken :: AuthToken
  , email :: EmailAddress
  , oldPassword :: HashedPassword
  , newPassword :: HashedPassword
  }

instance encodeJsonSecurityInitIn :: EncodeJson SecurityInitIn where
  encodeJson (SecurityInitIn {authToken,email,oldPassword,newPassword})
    =  "authToken" := authToken
    ~> "email" := Email.toString email
    ~> "newPassword" := newPassword
    ~> "oldPassword" := oldPassword
    ~> jsonEmptyObject

data SecurityInitOut
  = SecurityInitOutNoAuth
  | SecurityInitOutSuccess
  | SecurityInitOutFailure

derive instance genericSecurityInitOut :: Generic SecurityInitOut

instance showSecurityInitOut :: Show SecurityInitOut where
  show = gShow

instance decodeJsonSecurityInitOut :: DecodeJson SecurityInitOut where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          case unit of
            _ | s == "no-auth" -> pure SecurityInitOutNoAuth
              | s == "success" -> pure SecurityInitOutSuccess
              | s == "failure" -> pure SecurityInitOutFailure
              | otherwise -> fail "Not a SecurityInitOut"
    str


type SecuritySparrowClientQueues eff =
  SparrowStaticClientQueues eff SecurityInitIn SecurityInitOut
