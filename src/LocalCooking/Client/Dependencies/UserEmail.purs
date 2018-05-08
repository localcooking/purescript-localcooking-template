module LocalCooking.Client.Dependencies.UserEmail where

import LocalCooking.Client.Dependencies.AccessToken.Generic (AuthInitIn, AuthInitOut)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Data.Argonaut.JSONUnit (JSONUnit)
import Text.Email.Validate (EmailAddress)
import Sparrow.Client.Queue (SparrowStaticClientQueues)


type UserEmailInitIn = AuthInitIn AuthToken JSONUnit

type UserEmailInitOut = AuthInitOut EmailAddress


type UserEmailSparrowClientQueues eff =
  SparrowStaticClientQueues eff UserEmailInitIn UserEmailInitOut
