module LocalCooking.Auth.Storage where

import LocalCooking.Common.AuthToken (AuthToken)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Argonaut (jsonParser, decodeJson, encodeJson)
import Control.Monad.Eff (Eff)

import Browser.WebStorage (WEB_STORAGE, localStorage, getItem, setItem, removeItem, StorageKey (..))


authTokenKey :: StorageKey
authTokenKey = StorageKey "authToken"


getStoredAuthToken :: forall eff. Eff (webStorage :: WEB_STORAGE | eff) (Maybe AuthToken)
getStoredAuthToken = do
  mString <- getItem localStorage authTokenKey
  case mString of
    Nothing -> pure Nothing
    Just string -> case jsonParser string >>= decodeJson of
      Left e -> pure Nothing
      Right x -> pure (Just x)


storeAuthToken :: forall eff. AuthToken -> Eff (webStorage :: WEB_STORAGE | eff) Unit
storeAuthToken authToken =
  setItem localStorage authTokenKey $ show $ encodeJson authToken


clearAuthToken :: forall eff. Eff (webStorage :: WEB_STORAGE | eff) Unit
clearAuthToken =
  removeItem localStorage authTokenKey
