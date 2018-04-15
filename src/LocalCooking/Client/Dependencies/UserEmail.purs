module LocalCooking.Client.Dependencies.UserEmail where

import LocalCooking.Common.AuthToken (AuthToken)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Data.Generic (class Generic, gShow)
import Text.Email.Validate (EmailAddress, emailAddress)
import Control.Alternative ((<|>))
import Sparrow.Client.Queue (SparrowStaticClientQueues)



newtype UserEmailInitIn = UserEmailInitIn AuthToken

instance encodeJsonUserEmailInitIn :: EncodeJson UserEmailInitIn where
  encodeJson (UserEmailInitIn x) = "authToken" := x ~> jsonEmptyObject

data UserEmailInitOut
  = UserEmailInitOutNoAuth
  | UserEmailInitOutSuccess EmailAddress

derive instance genericUserEmailInitOut :: Generic UserEmailInitOut

instance showUserEmailInitOut :: Show UserEmailInitOut where
  show = gShow

instance decodeJsonUserEmailInitOut :: DecodeJson UserEmailInitOut where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          case unit of
            _ | s == "no-auth" -> pure UserEmailInitOutNoAuth
              | otherwise -> fail "Not a UserEmailInitOut"
        obj = do
          o <- decodeJson json
          e <- o .? "email"
          case emailAddress e of
            Nothing -> fail "Not a UserEmailInitOutSuccess"
            Just email -> pure (UserEmailInitOutSuccess email)
    str <|> obj


type UserEmailSparrowClientQueues eff =
  SparrowStaticClientQueues eff UserEmailInitIn UserEmailInitOut
