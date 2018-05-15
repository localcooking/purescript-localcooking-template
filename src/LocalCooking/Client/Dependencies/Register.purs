module LocalCooking.Client.Dependencies.Register where

import LocalCooking.Common.Password (HashedPassword)
import Google.ReCaptcha (ReCaptchaResponse)
import Facebook.Types (FacebookUserId)

import Prelude

import Sparrow.Client.Queue (SparrowStaticClientQueues)
import Text.Email.Validate (EmailAddress)
import Data.Generic (class Generic, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Data.Maybe (Maybe)
import Control.Alternative ((<|>))
import Text.Email.Validate as Email


newtype RegisterInitIn = RegisterInitIn
  { email     :: EmailAddress
  , password  :: HashedPassword
  , reCaptcha :: ReCaptchaResponse
  , fbUserId  :: Maybe FacebookUserId
  }

instance encodeJsonRegisterInitIn :: EncodeJson RegisterInitIn where
  encodeJson (RegisterInitIn {email,password,reCaptcha,fbUserId})
    =  "email"     := Email.toString email
    ~> "password"  := password
    ~> "reCaptcha" := reCaptcha
    ~> "fbUserId"  := fbUserId
    ~> jsonEmptyObject

data RegisterFailure
  = EmailExists

derive instance genericRegisterFailure :: Generic RegisterFailure

instance showRegisterFailure :: Show RegisterFailure where
  show = gShow

instance decodeJsonRegisterFailure :: DecodeJson RegisterFailure where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "email-exists" -> pure EmailExists
        | otherwise -> fail "Not a RegisterFailure"


data RegisterInitOut
  = RegisterInitOutEmailSent
  | RegisterInitOutBadCaptcha
  | RegisterInitOutDBError RegisterFailure

derive instance genericRegisterInitOut :: Generic RegisterInitOut

instance showRegisterInitOut :: Show RegisterInitOut where
  show = gShow

instance decodeJsonRegisterInitOut :: DecodeJson RegisterInitOut where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          case unit of
            _ | s == "email-sent" -> pure RegisterInitOutEmailSent
              | s == "bad-captcha" -> pure RegisterInitOutBadCaptcha
              | otherwise -> fail "Not a RegisterInitOut"
        obj = do
          o <- decodeJson json
          RegisterInitOutDBError <$> o .? "db"
    str <|> obj


type RegisterSparrowClientQueues eff =
  SparrowStaticClientQueues eff RegisterInitIn RegisterInitOut
