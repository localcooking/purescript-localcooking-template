module LocalCooking.Client.Dependencies.AccessToken.Generic where

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (.?), (~>), jsonEmptyObject, fail)
import Control.Alternative ((<|>))


newtype AuthInitIn k a = AuthInitIn
  { token :: k
  , subj  :: a
  }

instance encodeJsonAuthInitIn :: (EncodeJson k, EncodeJson a) => EncodeJson (AuthInitIn k a) where
  encodeJson (AuthInitIn {token,subj})
    =  "token" := token
    ~> "subj" := subj
    ~> jsonEmptyObject


data AuthInitOut a
  = AuthInitOutNoAuth
  | AuthInitOut
    { subj :: a
    }


instance decodeJsonAuthInitOut :: DecodeJson a => DecodeJson (AuthInitOut a) where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          if s == "no-auth"
            then pure AuthInitOutNoAuth
            else fail "Not a AuthInitOut"
        obj = do
          o <- decodeJson json
          subj <- o .? "subj"
          pure (AuthInitOut {subj})
    str <|> obj
