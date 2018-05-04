module Facebook.State where

import LocalCooking.Links.Class (class ToLocation, class FromLocation, toLocation, fromLocation)

import Prelude
import Data.Either (Either (..))
import Data.URI.Location (parseLocation, printLocation)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?), fail)
import Data.Generic (class Generic, gEq, gShow)
import Control.Alternative ((<|>))
import Text.Parsing.StringParser (runParser)
import Test.QuickCheck (class Arbitrary, arbitrary)


newtype FacebookLoginState siteLinks = FacebookLoginState
  { origin :: siteLinks
  }

derive instance genericFacebookLoginState :: Generic siteLinks => Generic (FacebookLoginState siteLinks)

instance eqFacebookLoginState :: Generic siteLinks => Eq (FacebookLoginState siteLinks) where
  eq = gEq

instance arbitraryFacebookLoginState :: Arbitrary siteLinks => Arbitrary (FacebookLoginState siteLinks) where
  arbitrary = do
    origin <- arbitrary
    pure $ FacebookLoginState {origin}

instance encodeJsonFacebookLoginState :: ToLocation siteLinks => EncodeJson (FacebookLoginState siteLinks) where
  encodeJson (FacebookLoginState {origin})
    =  "origin" := printLocation (toLocation origin)
    ~> jsonEmptyObject

instance decodeJsonFacebookLoginState :: FromLocation siteLinks => DecodeJson (FacebookLoginState siteLinks) where
  decodeJson json = do
    o <- decodeJson json
    s <- o .? "origin"
    case runParser parseLocation s of
      Left e -> fail (show e)
      Right loc -> case fromLocation loc of
        Left e -> fail e
        Right origin -> pure $ FacebookLoginState {origin}



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
