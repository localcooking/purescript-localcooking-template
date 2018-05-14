module LocalCooking.Client.Dependencies.UserRoles where

import LocalCooking.Client.Dependencies.AccessToken.Generic (AuthInitIn, AuthInitOut)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole)

import Data.Argonaut.JSONUnit (JSONUnit)
import Sparrow.Client.Queue (SparrowStaticClientQueues)


type UserRolesInitIn = AuthInitIn AuthToken JSONUnit

type UserRolesInitOut = AuthInitOut (Array UserRole)


type UserRolesSparrowClientQueues eff =
  SparrowStaticClientQueues eff UserRolesInitIn UserRolesInitOut
