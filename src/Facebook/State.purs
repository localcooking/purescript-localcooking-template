module Facebook.State where

import LocalCooking.Links.Class (class ToLocation, class FromLocation, toLocation, fromLocation)

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either (..))
import Data.URI.Location (parseLocation, printLocation)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?), fail)
import Data.Generic (class Generic, gEq, gShow)
import Data.NonEmpty (NonEmpty (..))
import Control.Alternative ((<|>))
import Text.Parsing.StringParser (runParser)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


newtype FacebookLoginState siteLinks = FacebookLoginState
  { origin :: siteLinks
  , formData :: Maybe FacebookLoginUnsavedFormData
  }

derive instance genericFacebookLoginState :: Generic siteLinks => Generic (FacebookLoginState siteLinks)

instance eqFacebookLoginState :: Generic siteLinks => Eq (FacebookLoginState siteLinks) where
  eq = gEq

instance arbitraryFacebookLoginState :: Arbitrary siteLinks => Arbitrary (FacebookLoginState siteLinks) where
  arbitrary = do
    origin <- arbitrary
    formData <- arbitrary
    pure $ FacebookLoginState {origin,formData}

instance encodeJsonFacebookLoginState :: ToLocation siteLinks => EncodeJson (FacebookLoginState siteLinks) where
  encodeJson (FacebookLoginState {origin,formData})
    =  "origin" := printLocation (toLocation origin)
    ~> "formData" := formData
    ~> jsonEmptyObject

instance decodeJsonFacebookLoginState :: FromLocation siteLinks => DecodeJson (FacebookLoginState siteLinks) where
  decodeJson json = do
    o <- decodeJson json
    formData <- o .? "formData"
    s <- o .? "origin"
    case runParser parseLocation s of
      Left e -> fail (show e)
      Right loc -> case fromLocation loc of
        Left e -> fail e
        Right origin -> pure $ FacebookLoginState {origin,formData}



data FacebookLoginUnsavedFormData
  = FacebookLoginUnsavedFormDataRegister
    { email :: String
    , emailConfirm :: String
    }
  | FacebookLoginUnsavedFormDataSecurity
    { email :: String
    , emailConfirm :: String
    }

derive instance genericFacebookLoginUnsavedFormData :: Generic FacebookLoginUnsavedFormData

instance showFacebookLoginUnsavedFormData :: Show FacebookLoginUnsavedFormData where
  show = gShow

instance arbitraryFacebookLoginUnsavedFormData :: Arbitrary FacebookLoginUnsavedFormData where
  arbitrary = oneOf $ NonEmpty
    ( do email <- arbitrary
         emailConfirm <- arbitrary
         pure $ FacebookLoginUnsavedFormDataRegister {email,emailConfirm}
    )
    [ do email <- arbitrary
         emailConfirm <- arbitrary
         pure $ FacebookLoginUnsavedFormDataSecurity {email,emailConfirm}
    ]

instance encodeJsonFacebookLoginUnsavedFormData :: EncodeJson FacebookLoginUnsavedFormData where
  encodeJson x = case x of
    FacebookLoginUnsavedFormDataRegister {email,emailConfirm}
      -> "register" :=
         ( "email" := email
         ~> "emailConfirm" := emailConfirm
         ~> jsonEmptyObject )
      ~> jsonEmptyObject
    FacebookLoginUnsavedFormDataSecurity {email,emailConfirm}
      -> "security" :=
         ( "email" := email
         ~> "emailConfirm" := emailConfirm
         ~> jsonEmptyObject )
      ~> jsonEmptyObject

instance decodeJsonFacebookLoginUnsavedFormData :: DecodeJson FacebookLoginUnsavedFormData where
  decodeJson json = do
    o <- decodeJson json
    let register = do
          o' <- o .? "register"
          email <- o' .? "email"
          emailConfirm <- o' .? "emailConfirm"
          pure (FacebookLoginUnsavedFormDataRegister {email,emailConfirm})
        security = do
          o' <- o .? "security"
          email <- o' .? "email"
          emailConfirm <- o' .? "emailConfirm"
          pure (FacebookLoginUnsavedFormDataSecurity {email,emailConfirm})
    register <|> security
