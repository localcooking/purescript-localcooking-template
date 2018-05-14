module LocalCooking.Main where

import LocalCooking.Spec (app)
import LocalCooking.Types.Env (Env)
import LocalCooking.Types.Params (LocalCookingParams)
import LocalCooking.Window (widthToWindowSize)
import LocalCooking.Auth.Storage (getStoredAuthToken, storeAuthToken, clearAuthToken)
import LocalCooking.Spec.Snackbar (SnackbarMessage (..), RedirectError (..), UserEmailError (..))
import LocalCooking.Links.Class (class LocalCookingSiteLinks, rootLink, registerLink, getUserDetailsLink, class ToLocation, class FromLocation, pushState', replaceState', onPopState, defaultSiteLinksToDocumentTitle)
import LocalCooking.Client.Dependencies.AuthToken (AuthTokenSparrowClientQueues, PreliminaryAuthToken (..))
import LocalCooking.Client.Dependencies.Register (RegisterSparrowClientQueues)
import LocalCooking.Client.Dependencies.UserEmail (UserEmailSparrowClientQueues)
import LocalCooking.Client.Dependencies.UserRoles (UserRolesSparrowClientQueues)
import LocalCooking.Client.Dependencies.Security (SecuritySparrowClientQueues)
import LocalCooking.Client.Dependencies.PasswordVerify (PasswordVerifySparrowClientQueues)
import LocalCooking.Client.Dependencies.AccessToken.Generic (AuthInitIn (..), AuthInitOut (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.User (class UserDetails)
import Facebook.State (FacebookLoginUnsavedFormData)

import Sparrow.Client (allocateDependencies, unpackClient)
import Sparrow.Client.Queue (newSparrowClientQueues, newSparrowStaticClientQueues, sparrowClientQueues, sparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.URI (Authority (..), Host (NameAddress), Scheme (..), Port (..))
import Data.URI.Location (Location, toURI)
import Data.String (takeWhile) as String
import Data.Int.Parse (parseInt, toRadix)
import Data.UUID (GENUUID)
import Data.Traversable (traverse_)
import Data.Time.Duration (Milliseconds (..))
import Data.Argonaut (jsonParser, decodeJson, encodeJson)
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Text.Email.Validate (EmailAddress)
import Control.Monad.Aff (ParAff, Aff, runAff_, delay, parallel)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
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
import Queue.Types (writeOnly)
import Queue (READ, WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import Browser.WebStorage (WEB_STORAGE, getItem, setItem, localStorage, StorageKey (..))
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



type LocalCookingArgs siteLinks userDetails eff =
  { content :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement
  , topbar ::
    { imageSrc :: Location
    , buttons :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement
    }
  , leftDrawer ::
    { buttons :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement
    }
  , userDetails ::
    { buttons :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement
    , content :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement
    , obtain  ::
      { email :: ParAff eff (Maybe EmailAddress)
      , roles :: ParAff eff (Array UserRole)
      } -> Aff eff (Maybe userDetails)
    }
  , deps          :: SparrowClientT eff (Eff eff) Unit
  , env           :: Env
  , initSiteLinks :: siteLinks
  , extraRedirect :: siteLinks -> Maybe userDetails -> Maybe siteLinks
  , palette ::
    { primary   :: ColorPalette
    , secondary :: ColorPalette
    }
  , extendedNetwork :: Array R.ReactElement
  }



-- | Top-level entry point to the application
defaultMain :: forall eff siteLinks userDetailsLinks userDetails
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => Eq siteLinks
            => ToLocation siteLinks
            => FromLocation siteLinks
            => Show siteLinks
            => UserDetails userDetails
            => LocalCookingArgs siteLinks userDetails (Effects eff)
            -> Eff (Effects eff) Unit
defaultMain
  { deps
  , topbar
  , leftDrawer
  , content
  , userDetails
  , env
  , initSiteLinks
  , palette
  , extendedNetwork
  , extraRedirect
  } = do
  -- inject events
  injectTapEvent
  _ <- registerShim


  -- DOM references
  w <- window
  l <- location w
  h <- history w
  d <- document w

  -- initial browser metadata values
  scheme <- Just <<< Scheme <<< String.takeWhile (\c -> c /= ':') <$> protocol l
  authority <- do
    host <- hostname l
    p' <- port l
    p <- case parseInt p' (toRadix 10) of
      Nothing ->  pure Nothing -- undefined <$ error "Somehow couldn't parse port"
      Just x -> pure (Just (Port x))
    pure $ Authority Nothing [Tuple (NameAddress host) p]




  -- Fetch the preliminary auth token from `env`, or LocalStorage
  ( preliminaryAuthToken :: PreliminaryAuthToken
    ) <- map PreliminaryAuthToken $ case env.authToken of
      PreliminaryAuthToken Nothing -> map Right <$> getStoredAuthToken
      PreliminaryAuthToken (Just eErrX) -> pure (Just eErrX)

  ( initFormDataRef :: Ref (Maybe FacebookLoginUnsavedFormData)
    ) <- newRef env.formData


  -- Global emitted snackbar messages
  ( errorMessageQueue :: One.Queue (read :: READ, write :: WRITE) (Effects eff) SnackbarMessage
    ) <- One.newQueue

  -- Global AuthToken value
  ( authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
    ) <- IxSignal.make Nothing

  -- Global userDetails value
  ( userDetailsSignal :: IxSignal (Effects eff) (Maybe userDetails)
    ) <- IxSignal.make Nothing



  -- Privacy policy - FIXME do this at registration
  privacyPolicyDialogQueue <- OneIO.newIOQueues
  let privacyPolicyKey = StorageKey "privacypolicy"
  mPrivPolicy <- getItem localStorage privacyPolicyKey
  let privPolicyUnread = case mPrivPolicy of
        Nothing -> true
        Just s -> case jsonParser s >>= decodeJson of
          Left _ -> true
          Right b -> b
  when privPolicyUnread $ do
    let go eX = case eX of
          Right (Just _) -> do
            log "setting privacy policy..."
            setItem localStorage privacyPolicyKey (show (encodeJson false))
          _ -> log "Failure in calling privacy policy queue?"
    runAff_ go $ do
      liftEff $ log "Calling..."
      delay $ Milliseconds 200.0
      OneIO.callAsync privacyPolicyDialogQueue unit


  -- Global current page value - for `back` compatibility while being driven by `siteLinksSignal`
  ( currentPageSignal :: IxSignal (Effects eff) siteLinks
    ) <- do
    initSiteLink <- do
      -- initial redirects
      let x = initSiteLinks
      case getUserDetailsLink x of
        Just _ -> do
          case preliminaryAuthToken of
            PreliminaryAuthToken Nothing -> do
              -- in /userDetails while not logged in
              void $ setTimeout 1000 $
                One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
              replaceState' (rootLink :: siteLinks) h
              setDocumentTitle d $ defaultSiteLinksToDocumentTitle $ rootLink :: siteLinks
              pure rootLink
            _ -> pure x
        _ | x == registerLink -> do
          case preliminaryAuthToken of
            PreliminaryAuthToken (Just (Right _)) -> do
              -- in /register while logged in
              void $ setTimeout 1000 $
                One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
              replaceState' (rootLink :: siteLinks) h
              setDocumentTitle d $ defaultSiteLinksToDocumentTitle $ rootLink :: siteLinks
              pure rootLink
            _ -> pure x
          | otherwise -> do
          mUserDetails <- IxSignal.get userDetailsSignal
          case extraRedirect x mUserDetails of
            Nothing -> pure x
            Just y -> do
              void $ setTimeout 1000 $
                One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
              replaceState' y h
              setDocumentTitle d (defaultSiteLinksToDocumentTitle y)
              pure y

    sig <- IxSignal.make initSiteLink

    -- handle back & forward
    flip onPopState w \(siteLink :: siteLinks) -> do
      let continue x = do
            setDocumentTitle d (defaultSiteLinksToDocumentTitle x)
            IxSignal.set x sig
      -- Top level redirect for browser back-button - no history change:
      case getUserDetailsLink siteLink of
        Just _ -> do
          mAuth <- IxSignal.get authTokenSignal
          case mAuth of
            Just _ -> continue siteLink
            Nothing -> do
              -- in /userDetails while not logged in
              void $ setTimeout 1000 $
                One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
              replaceState' (rootLink :: siteLinks) h
              continue rootLink
        _ | siteLink == registerLink -> do
            mAuth <- IxSignal.get authTokenSignal
            case mAuth of
              Nothing -> continue siteLink
              Just _ -> do
                -- in /register while logged in
                void $ setTimeout 1000 $
                  One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
                replaceState' (rootLink :: siteLinks) h
                continue rootLink
          | otherwise -> continue siteLink

    pure sig

  -- Global new page emitter & history driver - write to this to change the page.
  ( siteLinksSignal :: One.Queue (write :: WRITE) (Effects eff) siteLinks
    ) <- do
    q <- One.newQueue
    One.onQueue q \(siteLink :: siteLinks) -> do
      -- only respect changed pages
      y <- IxSignal.get currentPageSignal
      when (y /= siteLink) $ do
        let continue x = do
              pushState' x h
              setDocumentTitle d (defaultSiteLinksToDocumentTitle x)
              IxSignal.set x currentPageSignal
        -- redirect rules
        case getUserDetailsLink siteLink of
          Just _ -> do
            mAuth <- IxSignal.get authTokenSignal
            case mAuth of
              Just _ -> continue siteLink
              Nothing -> do
                -- in /userDetails while not logged in
                void $ setTimeout 1000 $
                  One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
                continue rootLink
          _ | siteLink == registerLink -> do
              mAuth <- IxSignal.get authTokenSignal
              case mAuth of
                Nothing -> continue siteLink
                Just _ -> do
                  -- in /register while logged in
                  void $ setTimeout 1000 $
                    One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
                  continue rootLink
            | otherwise -> continue siteLink
    pure (writeOnly q)


  onceRef <- newRef false
  -- rediect rules for async logout events
  let redirectOnAuth mAuth = do
        siteLink <- IxSignal.get currentPageSignal
        let continue = do
              One.putQueue siteLinksSignal rootLink
        case getUserDetailsLink siteLink of
          Just _ -> case mAuth of
            Nothing -> do
              -- hack for listening to the signal the first time on bind - first is always Nothing
              once <- do
                x <- readRef onceRef
                writeRef onceRef true
                pure x
              when once $ do
                void $ setTimeout 1000 $
                  One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
                continue
            _ -> pure unit
          _ | siteLink == registerLink -> case mAuth of
              Just _ -> do
                void $ setTimeout 1000 $
                  One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
                continue
              _ -> pure unit
            | otherwise -> pure unit
  IxSignal.subscribe redirectOnAuth authTokenSignal

  -- auth token storage and clearing on site-wide driven changes
  let localstorageOnAuth mAuth = case mAuth of
        Nothing -> clearAuthToken
        Just authToken -> storeAuthToken authToken
  IxSignal.subscribe localstorageOnAuth authTokenSignal


  -- Global window size value
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
  ( userRolesQueues :: UserRolesSparrowClientQueues (Effects eff)
    ) <- newSparrowStaticClientQueues
  ( securityQueues :: SecuritySparrowClientQueues (Effects eff)
    ) <- newSparrowStaticClientQueues
  ( passwordVerifyQueues :: PasswordVerifySparrowClientQueues (Effects eff)
    ) <- newSparrowStaticClientQueues
  allocateDependencies (scheme == Just (Scheme "https")) authority $ do
    unpackClient (Topic ["template", "authToken"]) (sparrowClientQueues authTokenQueues)
    unpackClient (Topic ["template", "register"]) (sparrowStaticClientQueues registerQueues)
    unpackClient (Topic ["template", "userEmail"]) (sparrowStaticClientQueues userEmailQueues)
    unpackClient (Topic ["template", "userRoles"]) (sparrowStaticClientQueues userRolesQueues)
    unpackClient (Topic ["template", "security"]) (sparrowStaticClientQueues securityQueues)
    unpackClient (Topic ["template", "passwordVerify"]) (sparrowStaticClientQueues passwordVerifyQueues)
    deps


  ( loginCloseQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
    ) <- writeOnly <$> One.newQueue

  -- user details fetcher and clearer
  let userDetailsOnAuth mAuth = case mAuth of
        Nothing -> IxSignal.set Nothing userDetailsSignal
        Just authToken -> do
          let resolve eX = case eX of
                Left _ -> do
                  IxSignal.set Nothing userDetailsSignal
                  One.putQueue errorMessageQueue (SnackbarMessageUserEmail UserEmailNoInitOut)
                Right mUserDetails -> do
                  IxSignal.set mUserDetails userDetailsSignal
                  One.putQueue loginCloseQueue unit

          runAff_ resolve $ userDetails.obtain
            { email: parallel $ do
                mInitOut <- OneIO.callAsync userEmailQueues (AuthInitIn {token: authToken, subj: JSONUnit})
                case mInitOut of
                  Nothing -> do
                    liftEff (One.putQueue errorMessageQueue (SnackbarMessageUserEmail UserEmailNoInitOut))
                    pure Nothing
                  Just initOut -> case initOut of
                    AuthInitOut {subj: email} -> pure (Just email)
                    AuthInitOutNoAuth -> do
                      liftEff (One.putQueue errorMessageQueue (SnackbarMessageUserEmail UserEmailNoAuth))
                      pure Nothing
            , roles: parallel $ do
                mInitOut <- OneIO.callAsync userRolesQueues (AuthInitIn {token: authToken, subj: JSONUnit})
                case mInitOut of
                  Nothing -> do
                    liftEff (One.putQueue errorMessageQueue (SnackbarMessageUserEmail UserEmailNoInitOut))
                    pure []
                  Just initOut -> case initOut of
                    AuthInitOut {subj: roles} -> pure roles
                    AuthInitOutNoAuth -> do
                      liftEff (One.putQueue errorMessageQueue (SnackbarMessageUserEmail UserEmailNoAuth))
                      pure []
            }
  IxSignal.subscribe userDetailsOnAuth authTokenSignal


  let params :: LocalCookingParams siteLinks userDetails (Effects eff)
      params =
        { toURI : \location -> toURI {scheme, authority: Just authority, location}
        , siteLinks : One.putQueue siteLinksSignal
        , currentPageSignal
        , windowSizeSignal
        , authTokenSignal
        , userDetailsSignal
        }

  -- Run User Interface
      props = unit
      {spec: reactSpec, dispatcher} =
        app
          params
          { development: env.development
          , preliminaryAuthToken
          , errorMessageQueue
          , privacyPolicyDialogQueue
          , loginCloseQueue
          , initFormDataRef
          , dependencies:
            { authTokenQueues
            , registerQueues
            , userEmailQueues
            , securityQueues
            , passwordVerifyQueues
            }
          , templateArgs:
            { content
            , topbar
            , leftDrawer
            , palette
            , userDetails:
              { buttons: userDetails.buttons
              , content: userDetails.content
              }
            }
          , env
          , extendedNetwork
          }
      component = R.createClass reactSpec
  traverse_ (render (R.createFactory component props) <<< htmlElementToElement) =<< body d

