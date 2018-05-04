module LocalCooking.Client.Dependencies.AuthToken where

import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import Facebook.Types (FacebookUserId, FacebookLoginReturnError)

import Sparrow.Client.Queue (SparrowClientQueues)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
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


data LoginFailure
  = BadPassword
  | EmailDoesntExist

derive instance genericLoginFailure :: Generic LoginFailure

instance showLoginFailure :: Show LoginFailure where
  show = gShow

instance decodeJsonLoginFailure :: DecodeJson LoginFailure where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "bad-password" -> pure BadPassword
        | s == "no-email" -> pure EmailDoesntExist
        | otherwise -> fail "Not a LoginFailure"


data AuthTokenFailure
  = FBLoginReturnBad String String
  | FBLoginReturnDenied String
  | FBLoginReturnBadParse
  | FBLoginReturnNoUser FacebookUserId
  | FBLoginReturnError FacebookLoginReturnError
  | AuthTokenLoginFailure LoginFailure
  | AuthExistsFailure

derive instance genericAuthTokenFailure :: Generic AuthTokenFailure

instance showAuthTokenFailure :: Show AuthTokenFailure where
  show = gShow

instance decodeJsonAuthTokenFailure :: DecodeJson AuthTokenFailure where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          case unit of
            _ | s == "bad-parse" -> pure FBLoginReturnBadParse
              | s == "exists-failure" -> pure AuthExistsFailure
              | otherwise -> fail "Not a AuthTokenFailure"
        obj = do
          o <- decodeJson json
          let bad = do
                o' <- o .? "fbBad"
                FBLoginReturnBad <$> o' .? "code" <*> o' .? "msg"
              denied = do
                o' <- o .? "fbDenied"
                FBLoginReturnDenied <$> o' .? "desc"
              failure = do
                AuthTokenLoginFailure <$> o .? "loginFailure"
              fbLoginReturnError = do
                FBLoginReturnError <$> o .? "fbLoginReturnError"
              noUser = do
                FBLoginReturnNoUser <$> o .? "no-user"
          bad <|> denied <|> failure <|> fbLoginReturnError <|> noUser
    str <|> obj



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
    str



type AuthTokenSparrowClientQueues eff =
  SparrowClientQueues eff AuthTokenInitIn AuthTokenInitOut AuthTokenDeltaIn AuthTokenDeltaOut



newtype PreliminaryAuthToken = PreliminaryAuthToken
  (Maybe (Either AuthTokenFailure AuthToken))

derive instance genericPreliminaryAuthToken :: Generic PreliminaryAuthToken

instance showPreliminaryAuthToken :: Show PreliminaryAuthToken where
  show = gShow

instance decodeJsonPreliminaryAuthToken :: DecodeJson PreliminaryAuthToken where
  decodeJson json = do
    mO <- decodeJson json
    case mO of
      Nothing -> pure (PreliminaryAuthToken Nothing)
      Just o -> do
        let err = Left <$> o .? "err"
            token = Right <$> o .? "token"
        (PreliminaryAuthToken <<< Just) <$> (err <|> token)
