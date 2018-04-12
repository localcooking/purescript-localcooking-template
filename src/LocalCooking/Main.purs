module LocalCooking.Main where

import LocalCooking.Spec (app)
import LocalCooking.Types.Env (Env)
import LocalCooking.Window (WindowSize, widthToWindowSize)
import LocalCooking.Auth.Storage (getStoredAuthToken, storeAuthToken, clearAuthToken)
import LocalCooking.Auth.Error (PreliminaryAuthToken (..))
import LocalCooking.Spec.Snackbar (SnackbarMessage (..), RedirectError (..))
import LocalCooking.Links.Class (class LocalCookingSiteLinks, rootLink, registerLink, isUserDetailsLink, class ToLocation, class FromLocation, pushState', replaceState', onPopState, toDocumentTitle)
import LocalCooking.Client.Dependencies.AuthToken (AuthTokenSparrowClientQueues)
import LocalCooking.Client.Dependencies.Register (RegisterSparrowClientQueues)
import LocalCooking.Client.Dependencies.UserEmail (UserEmailSparrowClientQueues)
import LocalCooking.Common.AuthToken (AuthToken)

import Sparrow.Client (allocateDependencies, unpackClient)
import Sparrow.Client.Queue (newSparrowClientQueues, newSparrowStaticClientQueues, sparrowClientQueues, sparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.URI (URI, Authority (..), Host (NameAddress), Scheme (..), Port (..))
import Data.URI.Location (Location, toURI)
import Data.String (takeWhile) as String
import Data.Int.Parse (parseInt, toRadix)
import Data.UUID (GENUUID)
import Data.Traversable (traverse_)
import Data.Time.Duration (Milliseconds (..))
import Text.Email.Validate (EmailAddress)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Execution.Immediate (SET_IMMEDIATE_SHIM, registerShim)

import Sparrow.Client.Types (SparrowClientT)
import React (ReactElement)
import React as R
import ReactDOM (render)
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT, injectTapEvent)
import MaterialUI.MuiThemeProvider (ColorPalette)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location, document, history)
import DOM.HTML.Location (hostname, protocol, port)
import DOM.HTML.Document (body)
import DOM.HTML.Document.Extra (setDocumentTitle)
import DOM.HTML.Types (HISTORY, htmlElementToElement)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Signal.Internal as Signal
import Signal.Time (debounce)
import Signal.DOM (windowDimensions)
import Queue (READ, WRITE)
import Queue.One as One
import Browser.WebStorage (WEB_STORAGE)
import WebSocket (WEBSOCKET)
import Network.HTTP.Affjax (AJAX)
import Crypto.Scrypt (SCRYPT)



type Effects eff =
  ( ref                :: REF
  , timer              :: TIMER
  , dom                :: DOM
  , now                :: NOW
  , scrypt             :: SCRYPT
  , history            :: HISTORY
  , console            :: CONSOLE
  , exception          :: EXCEPTION
  , webStorage         :: WEB_STORAGE
  , uuid               :: GENUUID
  , ajax               :: AJAX
  , ws                 :: WEBSOCKET
  , injectTapEvent     :: INJECT_TAP_EVENT
  , set_immediate_shim :: SET_IMMEDIATE_SHIM
  | eff)


type LocalCookingArgs siteLinks eff =
  { content :: { currentPageSignal :: IxSignal eff siteLinks
               , windowSizeSignal :: IxSignal eff WindowSize
               , siteLinks :: siteLinks -> Eff eff Unit
               , toURI :: Location -> URI
               , authTokenSignal :: IxSignal eff (Maybe AuthToken)
               , userEmailSignal :: IxSignal eff (Maybe EmailAddress)
               } -> Array ReactElement
  , topbar ::
    { imageSrc :: Location
    , buttons :: { toURI :: Location -> URI
                 , siteLinks :: siteLinks -> Eff eff Unit
                 , currentPageSignal :: IxSignal eff siteLinks
                 , windowSizeSignal :: IxSignal eff WindowSize
                 , authTokenSignal :: IxSignal eff (Maybe AuthToken)
                 , userEmailSignal :: IxSignal eff (Maybe EmailAddress)
                 } -> Array ReactElement
    }
  , leftDrawer ::
    { buttons :: { toURI :: Location -> URI
                 , siteLinks :: siteLinks -> Eff eff Unit
                 , currentPageSignal :: IxSignal eff siteLinks
                 , windowSizeSignal :: IxSignal eff WindowSize
                 , authTokenSignal :: IxSignal eff (Maybe AuthToken)
                 , userEmailSignal :: IxSignal eff (Maybe EmailAddress)
                 } -> Array ReactElement
    }
  , deps :: SparrowClientT eff (Eff eff) Unit
  , env :: Env
  , initSiteLinks :: siteLinks
  , palette :: {primary :: ColorPalette, secondary :: ColorPalette}
  }



defaultMain :: forall siteLinks eff
             . LocalCookingSiteLinks siteLinks
            => ToLocation siteLinks
            => FromLocation siteLinks
            => LocalCookingArgs siteLinks (Effects eff)
            -> Eff (Effects eff) Unit
defaultMain
  { deps
  , topbar
  , leftDrawer
  , content
  , env
  , initSiteLinks
  , palette
  } = do
  injectTapEvent
  _ <- registerShim


  w <- window
  l <- location w
  h <- history w
  d <- document w
  scheme <- Just <<< Scheme <<< String.takeWhile (\c -> c /= ':') <$> protocol l
  authority <- do
    host <- hostname l
    p' <- port l
    p <- case parseInt p' (toRadix 10) of
      Nothing ->  pure Nothing -- undefined <$ error "Somehow couldn't parse port"
      Just x -> pure (Just (Port x))
    pure $ Authority Nothing [Tuple (NameAddress host) p]


  ( preliminaryAuthToken :: PreliminaryAuthToken
    ) <- map PreliminaryAuthToken $ case env.authToken of
      PreliminaryAuthToken Nothing -> map Right <$> getStoredAuthToken
      PreliminaryAuthToken (Just eErrX) -> pure (Just eErrX)

  ( errorMessageQueue :: One.Queue (read :: READ, write :: WRITE) (Effects eff) SnackbarMessage
    ) <- One.newQueue

  ( authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
    ) <- IxSignal.make Nothing

  -- for `back` compatibility while being driven by `siteLinksSignal`
  ( currentPageSignal :: IxSignal (Effects eff) siteLinks
    ) <- do
    initSiteLink <- do
      -- initial redirects
      let x = initSiteLinks
      case preliminaryAuthToken of
        PreliminaryAuthToken (Just (Right _)) -> case unit of
          _ | x == registerLink -> do
              void $ setTimeout 1000 $
                One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
              replaceState' (rootLink :: siteLinks) h
              setDocumentTitle d $ toDocumentTitle $ rootLink :: siteLinks
              pure rootLink
            | otherwise -> pure x
        _ -> case unit of
          _ | isUserDetailsLink x -> do
              log "didn't replace state?"
              void $ setTimeout 1000 $
                One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
              replaceState' (rootLink :: siteLinks) h
              setDocumentTitle d $ toDocumentTitle $ rootLink :: siteLinks
              pure rootLink
            | otherwise -> pure x

    sig <- IxSignal.make initSiteLink
    flip onPopState w \(siteLink :: siteLinks) -> do
      let continue x = do
            setDocumentTitle d (toDocumentTitle x)
            IxSignal.set x sig
      -- Top level redirect for browser back-button - no history change:
      case unit of
        _ | siteLink == registerLink -> do
            mAuth <- IxSignal.get authTokenSignal
            case mAuth of
              Nothing -> continue siteLink
              Just _ -> do
                void $ setTimeout 1000 $
                  One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
                replaceState' (rootLink :: siteLinks) h
                continue rootLink
          | isUserDetailsLink siteLink -> do
            mAuth <- IxSignal.get authTokenSignal
            case mAuth of
              Just _ -> continue siteLink
              Nothing -> do
                void $ setTimeout 1000 $
                  One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
                replaceState' (rootLink :: siteLinks) h
                continue rootLink
          | otherwise -> continue siteLink

    pure sig

  -- history driver - write to this to change the page, with history.
  ( siteLinksSignal :: One.Queue (write :: WRITE) (Effects eff) siteLinks
    ) <- do
    q <- One.newQueue
    One.onQueue q \(siteLink :: siteLinks) -> do
      -- only respect changed pages
      y <- IxSignal.get currentPageSignal
      when (y /= siteLink) $ do
        let continue x = do
              pushState' x h
              setDocumentTitle d (toDocumentTitle x)
              IxSignal.set x currentPageSignal
        -- redirect rules
        case unit of
          _ | siteLink == registerLink -> do
              mAuth <- IxSignal.get authTokenSignal
              case mAuth of
                Nothing -> continue siteLink
                Just _ -> do
                  void $ setTimeout 1000 $
                    One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
                  continue rootLink
            | isUserDetailsLink siteLink -> do
              mAuth <- IxSignal.get authTokenSignal
              case mAuth of
                Just _ -> continue siteLink
                Nothing -> do
                  void $ setTimeout 1000 $
                    One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
                  continue rootLink
            | otherwise -> continue siteLink
    pure (One.writeOnly q)


  onceRef <- newRef false
  -- rediect for async logouts
  let redirectOnAuth mAuth = do
        siteLink <- IxSignal.get currentPageSignal
        let continue = One.putQueue siteLinksSignal rootLink
        case mAuth of
          Nothing -> do
            -- hack for listening to the signal the first time on bind
            once <- do
              x <- readRef onceRef
              writeRef onceRef true
              pure x
            when once $ case unit of
              _ | isUserDetailsLink siteLink -> do
                  void $ setTimeout 1000 $
                    One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
                  continue
                | otherwise -> pure unit
          Just _ -> case unit of
            _ | siteLink == registerLink -> do
                void $ setTimeout 1000 $
                  One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
                continue
              | otherwise -> pure unit
  IxSignal.subscribe redirectOnAuth authTokenSignal

  -- auth token storage and clearing on site-wide driven changes
  let localstorageOnAuth mAuth = case mAuth of
        Nothing -> clearAuthToken
        Just authToken -> storeAuthToken authToken
  IxSignal.subscribe localstorageOnAuth authTokenSignal


  windowSizeSignal <- do
    -- debounces and only relays when the window size changes
    sig <- debounce (Milliseconds 100.0) =<< windowDimensions
    initWidth <- (\w' -> w'.w) <$> Signal.get sig
    windowWidthRef <- newRef initWidth
    let initWindowSize = widthToWindowSize initWidth
    out <- IxSignal.make initWindowSize
    flip Signal.subscribe sig \w' -> do
      lastWindowWidth <- readRef windowWidthRef
      when (w'.w /= lastWindowWidth) $ do
        writeRef windowWidthRef w'.w
        let size = widthToWindowSize w'.w
        IxSignal.set size out
    pure out


  -- Sparrow dependencies
  ( authTokenQueues :: AuthTokenSparrowClientQueues (Effects eff)
    ) <- newSparrowClientQueues
  ( registerQueues :: RegisterSparrowClientQueues (Effects eff)
    ) <- newSparrowStaticClientQueues
  ( userEmailQueues :: UserEmailSparrowClientQueues (Effects eff)
    ) <- newSparrowStaticClientQueues
  allocateDependencies (scheme == Just (Scheme "https")) authority $ do
    unpackClient (Topic ["authToken"]) (sparrowClientQueues authTokenQueues)
    unpackClient (Topic ["register"]) (sparrowStaticClientQueues registerQueues)
    unpackClient (Topic ["userEmail"]) (sparrowStaticClientQueues userEmailQueues)
    deps


  -- Run User Interface
  let props = unit
      {spec: reactSpec, dispatcher} =
        app
          { toURI : \location -> toURI {scheme, authority: Just authority, location}
          , windowSizeSignal
          , currentPageSignal
          , siteLinks: One.putQueue siteLinksSignal
          , development: env.development
          , preliminaryAuthToken
          , errorMessageQueue
          , authTokenSignal
          , authTokenQueues
          , registerQueues
          , userEmailQueues
          , templateArgs: {content,topbar,leftDrawer,palette}
          , env
          }
      component = R.createClass reactSpec
  traverse_ (render (R.createFactory component props) <<< htmlElementToElement) =<< body d

