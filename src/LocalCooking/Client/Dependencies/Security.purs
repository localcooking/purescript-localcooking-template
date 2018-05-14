module LocalCooking.Client.Dependencies.Security where

import LocalCooking.Client.Dependencies.AccessToken.Generic (AuthInitIn, AuthInitOut)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Password (HashedPassword)

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject)
import Data.Generic (class Generic, gShow)
import Text.Email.Validate (EmailAddress)
import Text.Email.Validate as Email
import Sparrow.Client.Queue (SparrowStaticClientQueues)



newtype SecurityInitIn' = SecurityInitIn'
  { email :: EmailAddress
  , oldPassword :: HashedPassword
  , newPassword :: HashedPassword
  }

instance encodeJsonSecurityInitIn :: EncodeJson SecurityInitIn' where
  encodeJson (SecurityInitIn' {email,oldPassword,newPassword})
    =  "email" := Email.toString email
    ~> "newPassword" := newPassword
    ~> "oldPassword" := oldPassword
    ~> jsonEmptyObject

type SecurityInitIn = AuthInitIn AuthToken SecurityInitIn'

data SecurityInitOut'
  = SecurityInitOutSuccess
  | SecurityInitOutFailure

derive instance genericSecurityInitOut :: Generic SecurityInitOut'

instance showSecurityInitOut :: Show SecurityInitOut' where
  show = gShow

instance decodeJsonSecurityInitOut :: DecodeJson SecurityInitOut' where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          case unit of
            _ | s == "success" -> pure SecurityInitOutSuccess
              | s == "failure" -> pure SecurityInitOutFailure
              | otherwise -> fail "Not a SecurityInitOut"
    str

type SecurityInitOut = AuthInitOut SecurityInitOut'


type SecuritySparrowClientQueues eff =
  SparrowStaticClientQueues eff SecurityInitIn SecurityInitOut
