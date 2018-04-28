module LocalCooking.Auth.Error where

import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.?), fail, (:=), (~>), jsonEmptyObject)
import Data.Generic (class Generic, gShow, gEq)
import Data.NonEmpty ((:|))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


data AuthError
  = FBLoginReturnBad String String
  | FBLoginReturnDenied String
  | FBLoginReturnBadParse
  | FBLoginReturnNoUser
  | AuthExistsFailure

derive instance genericAuthError :: Generic AuthError

instance eqAuthError :: Eq AuthError where
  eq = gEq

instance showAuthError :: Show AuthError where
  show = gShow

instance arbitraryAuthError :: Arbitrary AuthError where
  arbitrary = oneOf $
        (FBLoginReturnBad <$> arbitrary <*> arbitrary)
    :|  [ FBLoginReturnDenied <$> arbitrary
        , pure FBLoginReturnBadParse
        , pure FBLoginReturnNoUser
        , pure AuthExistsFailure
        ]

instance encodeJsonAuthError :: EncodeJson AuthError where
  encodeJson x = case x of
    FBLoginReturnBad code msg -> "fbBad" := ("code" := code ~> "msg" := msg ~> jsonEmptyObject)
                              ~> jsonEmptyObject
    FBLoginReturnDenied desc -> "fbDenied" := ("desc" := desc ~> jsonEmptyObject)
                             ~> jsonEmptyObject
    FBLoginReturnBadParse -> encodeJson "bad-parse"
    FBLoginReturnNoUser -> encodeJson "no-user"
    AuthExistsFailure -> encodeJson "exists-failure"

instance decodeJsonAuthError :: DecodeJson AuthError where
  decodeJson json = do
    let obj = do
          o <- decodeJson json
          let bad = do
                o' <- o .? "fbBad"
                FBLoginReturnBad <$> o' .? "code" <*> o' .? "msg"
              denied = do
                o' <- o .? "fbDenied"
                FBLoginReturnDenied <$> o' .? "desc"
          bad <|> denied
        str = do
          s <- decodeJson json
          case unit of
            _ | s == "bad-parse" -> pure FBLoginReturnBadParse
              | s == "no-user" -> pure FBLoginReturnNoUser
              | s == "exists-failure" -> pure AuthExistsFailure
              | otherwise -> fail "Not a AuthError"
    obj <|> str



newtype PreliminaryAuthToken = PreliminaryAuthToken
  (Maybe (Either AuthError AuthToken))

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
