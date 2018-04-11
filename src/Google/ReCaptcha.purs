module Google.ReCaptcha where

import Prelude ((<$>), class Eq, class Show)
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)



newtype ReCaptchaResponse = ReCaptchaResponse String

derive instance genericReCaptchaResponse :: Generic ReCaptchaResponse

instance eqReCaptchaResponse :: Eq ReCaptchaResponse where
  eq = gEq

instance showReCaptchaResponse :: Show ReCaptchaResponse where
  show = gShow

instance encodeJsonReCaptchaResponse :: EncodeJson ReCaptchaResponse where
  encodeJson (ReCaptchaResponse x) = encodeJson x

instance decodeJsonReCaptchaResponse :: DecodeJson ReCaptchaResponse where
  decodeJson json = ReCaptchaResponse <$> decodeJson json
