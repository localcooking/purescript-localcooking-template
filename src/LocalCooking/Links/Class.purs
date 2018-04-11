module LocalCooking.Links.Class where

import Prelude
import Data.URI.Location (Location (..), fromURI, printLocation, parseLocation)
import Data.URI.Location as Location
import Data.Either (Either (..))
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Argonaut (encodeJson, decodeJson)
import Text.Parsing.StringParser (runParser)
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

class Eq siteLinks <= LocalCookingSiteLinks siteLinks where
  rootLink :: siteLinks
  registerLink :: siteLinks
  userDetailsLink :: siteLinks
  isUserDetailsLink :: siteLinks -> Boolean
  toDocumentTitle :: siteLinks -> DocumentTitle



pushState' :: forall eff siteLinks
            . ToLocation siteLinks
           => LocalCookingSiteLinks siteLinks
           => siteLinks -> History -> Eff (history :: HISTORY | eff) Unit
pushState' x h = do
  pushState
    (toForeign $ encodeJson $ printLocation $ toLocation x)
    (toDocumentTitle x)
    (URL $ Location.printLocation $ toLocation x)
    h


replaceState' :: forall eff siteLinks
               . ToLocation siteLinks
              => LocalCookingSiteLinks siteLinks
              => siteLinks -> History -> Eff (history :: HISTORY | eff) Unit
replaceState' x h = do
  replaceState
    (toForeign $ encodeJson $ printLocation $ toLocation x)
    (toDocumentTitle x)
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

