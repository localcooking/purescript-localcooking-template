module LocalCooking.Links where

import LocalCooking.Links.Class (class ToLocation)

import Prelude
import Data.URI.Location (Location (..))
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Path.Pathy ((</>), dir, file, rootDir)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (char, string, eof)



data ThirdPartyLoginReturnLinks
  = FacebookLoginReturn -- (Maybe {code :: String, state :: Maybe Unit}) -- FIXME hardcode a facebook login state

instance toLocationThirdPartyLoginReturnLinks :: ToLocation ThirdPartyLoginReturnLinks where
  toLocation x = case x of
    FacebookLoginReturn -> Location (Right $ rootDir </> file "facebookLoginReturn") Nothing Nothing


thirdPartyLoginReturnLinksParser :: Parser ThirdPartyLoginReturnLinks
thirdPartyLoginReturnLinksParser = do
  let facebook = do
        void divider
        FacebookLoginReturn <$ (string "facebookLoginReturn" *> eof)
  facebook
  where
    divider = char '/'



data ImageLinks
  = LogoPng
  | Logo40Png
  | LogoWhitePng
  | LogoWhite40Png
  | IconPng
  | IconSvg


instance toLocationImageLinks :: ToLocation ImageLinks where
  toLocation x = case x of
    LogoPng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo.png") Nothing Nothing
    Logo40Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-40.png") Nothing Nothing
    LogoWhitePng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-white.png") Nothing Nothing
    LogoWhite40Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-white-40.png") Nothing Nothing
    IconPng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "icon.png") Nothing Nothing
    IconSvg -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "icon.svg") Nothing Nothing


data PolicyLinks
  = PrivacyPolicyLink

instance toLocationPolicyLinks :: ToLocation PolicyLinks where
  toLocation x = case x of
    PrivacyPolicyLink -> Location (Right $ rootDir </> file "privacypolicy.html") Nothing Nothing
