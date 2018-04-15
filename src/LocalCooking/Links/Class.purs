module LocalCooking.Links.Class where

import Prelude
import Data.URI.Location (Location, printLocation, parseLocation)
import Data.URI.Location as Location
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Argonaut (encodeJson, decodeJson)
import Type.Proxy (Proxy (..))
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.String (string, char, eof)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Uncurried (mkEffFn1, runEffFn2)
import DOM (DOM)
import DOM.HTML.History (DocumentTitle (..), pushState, replaceState, URL (..))
import DOM.HTML.Window.Extra (onPopStateImpl)
import DOM.HTML.Types (History, HISTORY, Window)



class ToLocation sym where
  toLocation :: sym -> Location

class FromLocation sym where
  fromLocation :: Location -> Either String sym


class Eq userDetailsLinks <= LocalCookingUserDetailsLinks userDetailsLinks where
  userDetailsGeneralLink :: userDetailsLinks
  userDetailsSecurityLink :: userDetailsLinks
  toUserDetailsDocumentTitle :: userDetailsLinks -> String -- ^ The prefix, i.e. `Security - `


class ( Eq siteLinks
      , LocalCookingUserDetailsLinks userDetailsLinks
      ) <= LocalCookingSiteLinks siteLinks userDetailsLinks
           | siteLinks -> userDetailsLinks
           , userDetailsLinks -> siteLinks where
  rootLink :: siteLinks
  registerLink :: siteLinks
  userDetailsLink :: Maybe userDetailsLinks -> siteLinks
  getUserDetailsLink :: siteLinks -> Maybe (Maybe userDetailsLinks)
  toDocumentTitle :: siteLinks -> String -- ^ The prefix, i.e. `Register - `
  subsidiaryTitle :: Proxy siteLinks -> String -- ^ The suffix, i.e. ` Chefs`


defaultSiteLinksPathParser :: forall siteLinks userDetailsLinks
                            . LocalCookingSiteLinks siteLinks userDetailsLinks
                           => Parser userDetailsLinks -> Parser siteLinks
defaultSiteLinksPathParser userDetailsLinksParser = do
  let root = rootLink <$ eof
      register = do
        void (string "register")
        pure registerLink
      userDetails = do
        void (string "userDetails")
        mUserDetails <- optionMaybe (divider *> userDetailsLinksParser)
        pure (userDetailsLink mUserDetails)
  try register
    <|> try userDetails
    <|> root
  where
    divider = char '/'


defaultSiteLinksToDocumentTitle :: forall siteLinks userDetailsLinks
                                 . LocalCookingSiteLinks siteLinks userDetailsLinks
                                => Eq siteLinks
                                => siteLinks
                                -> DocumentTitle
defaultSiteLinksToDocumentTitle link =
  DocumentTitle $ case getUserDetailsLink link of
    Just mDetails ->
      let x = case mDetails of
                Nothing -> ""
                Just d -> toUserDetailsDocumentTitle d
      in  x <> "User Details - " <> docT
    _ | link == rootLink -> docT
      | link == registerLink -> "Register - " <> docT
      | otherwise -> toDocumentTitle link <> docT
  where
    docT = "Local Cooking" <> subsidiaryTitle (Proxy :: Proxy siteLinks)



pushState' :: forall eff siteLinks userDetailsLinks
            . ToLocation siteLinks
           => Eq siteLinks
           => LocalCookingSiteLinks siteLinks userDetailsLinks
           => siteLinks -> History -> Eff (history :: HISTORY | eff) Unit
pushState' x h = do
  pushState
    (toForeign $ encodeJson $ printLocation $ toLocation x)
    (defaultSiteLinksToDocumentTitle x)
    (URL $ Location.printLocation $ toLocation x)
    h


replaceState' :: forall eff siteLinks userDetailsLinks
               . ToLocation siteLinks
              => Eq siteLinks
              => LocalCookingSiteLinks siteLinks userDetailsLinks
              => siteLinks -> History -> Eff (history :: HISTORY | eff) Unit
replaceState' x h = do
  replaceState
    (toForeign $ encodeJson $ printLocation $ toLocation x)
    (defaultSiteLinksToDocumentTitle x)
    (URL $ Location.printLocation $ toLocation x)
    h


onPopState :: forall eff siteLinks
            . FromLocation siteLinks
           => (siteLinks -> Eff (dom :: DOM, exception :: EXCEPTION | eff) Unit)
           -> Window
           -> Eff (dom :: DOM, exception :: EXCEPTION | eff) Unit
onPopState go w =
  onPopState' \fgn -> case decodeJson (unsafeFromForeign fgn) of
    Left e -> throw e
    Right str -> case runParser parseLocation str of
      Left e -> throw (show e)
      Right loc -> case fromLocation loc of
        Left e -> throw e
        Right (x :: siteLinks) -> go x
  where
    onPopState' f = runEffFn2 onPopStateImpl (mkEffFn1 f) w

