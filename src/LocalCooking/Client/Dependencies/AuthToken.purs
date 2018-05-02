module LocalCooking.Client.Dependencies.AuthToken where

import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Sparrow.Client.Queue (SparrowClientQueues)

import Prelude
import Data.Argonaut (class EncodeJson, encodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?), fail)
import Data.Generic (class Generic, gShow)
import Control.Alternative ((<|>))
import Text.Email.Validate (EmailAddress)
import Text.Email.Validate as Email



data AuthTokenInitIn
  = AuthTokenInitInLogin
    { email :: EmailAddress
    , password :: HashedPassword
    }
  | AuthTokenInitInExists
    { exists :: AuthToken
    }

instance encodeJsonAuthTokenInitIn :: EncodeJson AuthTokenInitIn where
  encodeJson x = case x of
    AuthTokenInitInLogin {email,password}
      -> "email" := Email.toString email
      ~> "password" := password
      ~> jsonEmptyObject
    AuthTokenInitInExists {exists}
      -> "exists" := exists
      ~> jsonEmptyObject


data AuthTokenFailure
  = BadPassword
  | EmailDoesntExist

derive instance genericAuthTokenFailure :: Generic AuthTokenFailure

instance showAuthTokenFailure :: Show AuthTokenFailure where
  show = gShow

instance decodeJsonAuthTokenFailure :: DecodeJson AuthTokenFailure where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "bad-password" -> pure BadPassword
        | s == "no-email" -> pure EmailDoesntExist
        | otherwise -> fail "Not a AuthTokenFailure"


data AuthTokenInitOut
  = AuthTokenInitOutFailure AuthTokenFailure
  | AuthTokenInitOutSuccess AuthToken

instance decodeJsonAuthTokenInitOut :: DecodeJson AuthTokenInitOut where
  decodeJson json = do
    o <- decodeJson json
    let failure = AuthTokenInitOutFailure <$> o .? "failure"
        success = AuthTokenInitOutSuccess <$> o .? "success"
    success <|> failure


data AuthTokenDeltaIn
  = AuthTokenDeltaInLogout

instance encodeJsonAuthTokenDeltaIn :: EncodeJson AuthTokenDeltaIn where
  encodeJson AuthTokenDeltaInLogout = encodeJson "logout"


data AuthTokenDeltaOut
  = AuthTokenDeltaOutRevoked

instance decodeJsonAuthTokenDeltaOut :: DecodeJson AuthTokenDeltaOut where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          case unit of
            _ | s == "revoked" -> pure AuthTokenDeltaOutRevoked
              | otherwise -> fail "Not a AuthTokenDeltaOut"
    obj <|> str



type AuthTokenSparrowClientQueues eff =
  SparrowClientQueues eff AuthTokenInitIn AuthTokenInitOut AuthTokenDeltaIn AuthTokenDeltaOut
