module LocalCooking.Client.Dependencies.PasswordVerify where

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



data PasswordVerifyInitIn
  = PasswordVerifyInitInAuth
    { authToken :: AuthToken
    , password  :: HashedPassword
    }
  | PasswordVerifyInitInUnauth
    { email    :: EmailAddress
    , password :: HashedPassword
    }

instance encodeJsonPasswordVerifyInitIn :: EncodeJson PasswordVerifyInitIn where
  encodeJson initIn = case initIn of
    PasswordVerifyInitInAuth {authToken,password}
      ->  "authToken" := authToken
      ~> "password" := password
      ~> jsonEmptyObject
    PasswordVerifyInitInUnauth {email,password}
      ->  "email" := Email.toString email
      ~> "password" := password
      ~> jsonEmptyObject

data PasswordVerifyInitOut
  = PasswordVerifyInitOutNoAuth
  | PasswordVerifyInitOutSuccess
  | PasswordVerifyInitOutFailure

derive instance genericPasswordVerifyInitOut :: Generic PasswordVerifyInitOut

instance showPasswordVerifyInitOut :: Show PasswordVerifyInitOut where
  show = gShow

instance decodeJsonPasswordVerifyInitOut :: DecodeJson PasswordVerifyInitOut where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          case unit of
            _ | s == "no-auth" -> pure PasswordVerifyInitOutNoAuth
              | s == "success" -> pure PasswordVerifyInitOutSuccess
              | s == "failure" -> pure PasswordVerifyInitOutFailure
              | otherwise -> fail "Not a PasswordVerifyInitOut"
    str


type PasswordVerifySparrowClientQueues eff =
  SparrowStaticClientQueues eff PasswordVerifyInitIn PasswordVerifyInitOut
