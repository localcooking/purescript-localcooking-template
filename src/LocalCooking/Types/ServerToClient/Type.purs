module LocalCooking.Types.ServerToClient.Type where

import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Dependencies.AuthToken (PreliminaryAuthToken)
import LocalCooking.Semantics.Common (ConfirmEmailError)
import Facebook.State (FacebookLoginUnsavedFormData)
import Facebook.Types (FacebookClientId)
import Google.ReCaptcha (ReCaptchaSiteKey)

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either (..))
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (Json, class DecodeJson, class EncodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject)
import Data.Typelevel.Undefined (undefined)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Console (error)
import Test.QuickCheck (class Arbitrary, arbitrary)


newtype ServerToClient = ServerToClient
  { development            :: Boolean
  , facebookClientId       :: FacebookClientId
  , googleReCaptchaSiteKey :: ReCaptchaSiteKey
  , confirmEmail           :: Maybe ConfirmEmailError
  , authToken              :: Maybe PreliminaryAuthToken
  , formData               :: Maybe FacebookLoginUnsavedFormData
  , salt                   :: HashedPassword
  }

derive instance genericServerToClient :: Generic ServerToClient

instance showServerToClient :: Show ServerToClient where
  show = gShow

instance eqServerToClient :: Eq ServerToClient where
  eq = gEq

instance arbitraryServerToClient :: Arbitrary ServerToClient where
  arbitrary = do
    development <- arbitrary
    facebookClientId <- arbitrary
    googleReCaptchaSiteKey <- arbitrary
    confirmEmail <- arbitrary
    authToken <- arbitrary
    formData <- arbitrary
    salt <- arbitrary
    pure $ ServerToClient
      { development
      , facebookClientId
      , googleReCaptchaSiteKey
      , confirmEmail
      , authToken
      , formData
      , salt
      }

instance encodeJsonServerToClient :: EncodeJson ServerToClient where
  encodeJson (ServerToClient {development,facebookClientId,googleReCaptchaSiteKey,confirmEmail,authToken,formData,salt})
    =  "development" := development
    ~> "facebookClientId" := facebookClientId
    ~> "googleReCaptchaSiteKey" := googleReCaptchaSiteKey
    ~> "confirmEmail" := confirmEmail
    ~> "authToken" := authToken
    ~> "formData" := formData
    ~> "salt" := salt
    ~> jsonEmptyObject

instance decodeJsonServerToClient :: DecodeJson ServerToClient where
  decodeJson json = do
    o <- decodeJson json
    development <- o .? "development"
    facebookClientId <- o .? "facebookClientId"
    googleReCaptchaSiteKey <- o .? "googleReCaptchaSiteKey"
    confirmEmail <- o .? "confirmEmail"
    authToken <- o .? "authToken"
    formData <- o .? "formData"
    salt <- o .? "salt"
    pure $ ServerToClient
      { development
      , facebookClientId
      , googleReCaptchaSiteKey
      , confirmEmail
      , authToken
      , formData
      , salt
      }
