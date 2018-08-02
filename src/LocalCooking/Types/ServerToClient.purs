module LocalCooking.Types.ServerToClient
  ( module Type
  , serverToClient
  , env
  , getInitPreliminarySessionToken
  ) where

import LocalCooking.Types.ServerToClient.Type (ServerToClient (..))
import LocalCooking.Types.ServerToClient.Type as Type
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Global.Error (LoginError)
import Auth.AccessToken.Session (PreliminarySessionToken (..))
import Auth.AccessToken.Session.Storage (getStoredSessionToken)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.JSONEither (JSONEither (JSONRight))
import Data.Typelevel.Undefined (undefined)
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Console (error)
import Browser.WebStorage (WEB_STORAGE)



foreign import serverToClientImpl :: Json

serverToClient :: ServerToClient
serverToClient = unsafePerformEff $ case decodeJson serverToClientImpl of
  Right x -> pure x
  Left e -> undefined <$ error e


env :: Env
env = case serverToClient of
  ServerToClient {development,facebookClientId,googleReCaptchaSiteKey,salt} ->
    { development
    , facebookClientId
    , googleReCaptchaSiteKey
    , salt
    }


-- | Fetch the preliminary auth token from `env`, or LocalStorage
getInitPreliminarySessionToken :: forall eff
                                . Eff (webStorage :: WEB_STORAGE | eff)
                                    (Maybe (PreliminarySessionToken LoginError))
getInitPreliminarySessionToken = case serverToClient of
  ServerToClient {sessionToken} -> case sessionToken of
    Nothing -> do
      mTkn <- getStoredSessionToken
      pure (PreliminarySessionToken <<< JSONRight <$> mTkn)
    tkn -> pure tkn
