module Data.Argonaut.JSONUnit where

import Prelude
import Data.Generic (class Generic, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)


data JSONUnit = JSONUnit

derive instance genericJSONUnit :: Generic JSONUnit

instance eqJSONUnit :: Eq JSONUnit where
  eq = gEq

instance encodeJsonJSONUnit :: EncodeJson JSONUnit where
  encodeJson JSONUnit = encodeJson ""

instance decodeJsonJSONUnit :: DecodeJson JSONUnit where
  decodeJson json = do
    s <- decodeJson json
    if s == ""
      then pure JSONUnit
      else fail "Not a JSONUnit"
