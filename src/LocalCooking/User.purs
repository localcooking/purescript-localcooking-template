module LocalCooking.User where

import Text.Email.Validate (EmailAddress)


class UserDetails userDetails where
  getEmailAddress :: userDetails -> EmailAddress
