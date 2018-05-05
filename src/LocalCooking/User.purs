module LocalCooking.User where

import Text.Email.Validate (EmailAddress)
import LocalCooking.Common.User.Role (UserRole)


class UserDetails userDetails where
  getEmailAddress :: userDetails -> EmailAddress
  getUserRoles :: userDetails -> Array UserRole
