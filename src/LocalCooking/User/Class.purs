module LocalCooking.User.Class where

import Text.Email.Validate (EmailAddress)
import LocalCooking.Common.User.Role (UserRole)


-- | Fields assumed to be supported by the subsidiary site's user details -
-- | additional fields might include customer details, chef details, etc.
class UserDetails userDetails where
  getEmailAddress :: userDetails -> EmailAddress
  getUserRoles :: userDetails -> Array UserRole
