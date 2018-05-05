module LocalCooking.Client.Dependencies.UserRoles where

import LocalCooking.Client.Dependencies.AccessToken.Generic (AuthInitIn, AuthInitOut)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Generic (class Generic, gShow)
import Control.Alternative ((<|>))
import Sparrow.Client.Queue (SparrowStaticClientQueues)


type UserRolesInitIn = AuthInitIn AuthToken JSONUnit

type UserRolesInitOut = AuthInitOut (Array UserRole)


type UserRolesSparrowClientQueues eff =
  SparrowStaticClientQueues eff UserRolesInitIn UserRolesInitOut
