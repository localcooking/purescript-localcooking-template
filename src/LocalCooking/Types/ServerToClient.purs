module LocalCooking.Types.ServerToClient
  ( module Type
  , serverToClient
  , env
  ) where

import LocalCooking.Types.ServerToClient.Type (ServerToClient (..))
import LocalCooking.Types.ServerToClient.Type as Type
import LocalCooking.Spec.Types.Env (Env)

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either (..))
import Data.Generic (class Generic, gShow, gEq)
import Data.Argonaut (Json, class DecodeJson, class EncodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject)
import Data.Typelevel.Undefined (undefined)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Console (error)



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
