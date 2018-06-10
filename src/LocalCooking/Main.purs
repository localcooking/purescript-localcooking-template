module LocalCooking.Main where

import LocalCooking.Spec (app)
-- import LocalCooking.Spec.Dialogs (newDialogQueues)
import LocalCooking.Spec.Content.Register (RegisterUnsavedFormData (..))
import LocalCooking.Spec.Content.UserDetails.Security (SecurityUnsavedFormData (..))
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Types.ServerToClient (ServerToClient (..), serverToClient)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Auth.Storage (getStoredAuthToken, storeAuthToken, clearAuthToken)
-- import LocalCooking.Spec.Snackbar (GlobalError (..), RedirectError (..), UserEmailError (..))
import LocalCooking.Global.Error
  (GlobalError (..), RedirectError (..), UserEmailError (..), AuthTokenFailure (..))
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks, rootLink, registerLink, getUserDetailsLink, pushState', replaceState', onPopState, defaultSiteLinksToDocumentTitle)
import LocalCooking.Global.User.Class (class UserDetails)
import LocalCooking.Dependencies (dependencies, newQueues, DependenciesQueues)
import LocalCooking.Dependencies.AuthToken (PreliminaryAuthToken (..), AuthTokenDeltaOut (..), AuthTokenInitOut (..), AuthTokenDeltaIn (..), AuthTokenInitIn (..))
import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
-- import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Semantics.Common (User)
import Facebook.State (FacebookLoginUnsavedFormData (..))

import Sparrow.Client (allocateDependencies, unpackClient)
import Sparrow.Client.Queue (newSparrowClientQueues, newSparrowStaticClientQueues, sparrowClientQueues, sparrowStaticClientQueues, mountSparrowClientQueuesSingleton)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.URI (Authority (..), Host (NameAddress), Scheme (..), Port (..))
import Data.URI.Location (Location, toURI, class ToLocation, class FromLocation)
import Data.String (takeWhile) as String
import Data.Int.Parse (parseInt, toRadix)
import Data.UUID (GENUUID)
import Data.Traversable (traverse_)
import Data.Time.Duration (Milliseconds (..))
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Data.Generic (class Generic)
-- import Text.Email.Validate (EmailAddress)
import Control.Monad.Aff (ParAff, Aff, runAff_, parallel)
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
import DOM.HTML.Window.Extra (widthToWindowSize)
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



type LocalCookingArgs siteLinks userDetails siteQueues eff =
  { content :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement -- ^ Primary content process
  , topbar ::
    { imageSrc :: Location -- ^ Nify colored logo
    , buttons :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement
    }
  , leftDrawer ::
    { buttons :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement -- ^ Mobile only
    }
  , userDetails ::
    { buttons :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement -- ^ Side navigation
    , content :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement
    , obtain  :: -- ^ Given a method to obtain a few fields, obtain the entire struct
      { user  :: ParAff eff (Maybe User)
      -- , roles :: ParAff eff (Array UserRole)
      } -> Aff eff (Maybe userDetails)
    }
  , newSiteQueues :: Eff eff siteQueues -- ^ New subsidiary-site specific sparrow dependency queues
  , deps          :: siteQueues -> SparrowClientT eff (Eff eff) Unit -- ^ Apply those queues -- FIXME TODO MonadBaseControl?
  , initSiteLinks :: siteLinks -- ^ The page opened
  , extraRedirect :: siteLinks -> Maybe userDetails -> Maybe siteLinks -- ^ Additional redirection rules per-site
  , palette :: -- ^ Colors
    { primary   :: ColorPalette
    , secondary :: ColorPalette
    }
  , extendedNetwork :: Array R.ReactElement -- ^ Buttons
  , env :: Env -- ^ Provided by Server.FrontendEnv
  }



-- | Top-level entry point to the application
defaultMain :: forall eff siteLinks userDetailsLinks userDetails siteQueues
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => Eq siteLinks
            => ToLocation siteLinks
            => FromLocation siteLinks
            => Show siteLinks
            => UserDetails userDetails
            => Generic siteLinks
            => Generic userDetails
            => LocalCookingArgs siteLinks userDetails siteQueues (Effects eff)
            -> Eff (Effects eff) Unit
defaultMain
  { newSiteQueues
  , deps
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

  -- given initial values from browser metadata
  scheme <- Just <<< Scheme <<< String.takeWhile (\c -> c /= ':') <$> protocol l
  authority <- do
    host <- hostname l
    p' <- port l
    p <- case parseInt p' (toRadix 10) of
      Nothing ->  pure Nothing -- undefined <$ error "Somehow couldn't parse port"
      Just x -> pure (Just (Port x))
    pure (Authority Nothing [Tuple (NameAddress host) p])



  -- Mutable form data reference for first-loaded component that obtains it
  -- FIXME this is fucked; shouldn't really need a ref honestly
  -- ( initFormDataRef :: Ref (Maybe FacebookLoginUnsavedFormData)
  --   ) <- newRef env.formData -- parsed from query string by server
  securityUnsavedFormData <- writeOnly <$> One.newQueue
  registerUnsavedFormData <- writeOnly <$> One.newQueue
  void $ setTimeout 1000 $ do
    case serverToClient of
      ServerToClient {formData} -> case formData of
        Nothing -> pure unit
        Just unsavedFormData -> case unsavedFormData of
          FacebookLoginUnsavedFormDataSecurity xs ->
            One.putQueue securityUnsavedFormData (SecurityUnsavedFormData xs)
          FacebookLoginUnsavedFormDataRegister xs ->
            One.putQueue registerUnsavedFormData (RegisterUnsavedFormData xs)


  -- Global emitted snackbar messages
  ( globalErrorQueue :: One.Queue (read :: READ, write :: WRITE) (Effects eff) GlobalError
    ) <- One.newQueue

  -- Global AuthToken value -- FIXME start as Nothing? Can't pack PreliminaryAuthToken on processing?
  ( authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
    ) <- IxSignal.make Nothing

  -- Global userDetails value -- FIXME ditto
  ( userDetailsSignal :: IxSignal (Effects eff) (Maybe userDetails)
    ) <- IxSignal.make Nothing

  -- Fetch the preliminary auth token from `env`, or LocalStorage
  ( preliminaryAuthToken :: Maybe PreliminaryAuthToken
    ) <- case serverToClient of
      ServerToClient {authToken} -> case authToken of
        Nothing -> do
          mTkn <- getStoredAuthToken
          pure (PreliminaryAuthToken <<< Right <$> mTkn)
        tkn -> pure tkn

  log $ "Preliminary auth token: " <> show preliminaryAuthToken


  -- Spit out confirm email message if it exists
  case serverToClient of
    ServerToClient {confirmEmail} -> case confirmEmail of
      Nothing -> pure unit
      Just confEmail -> void $ setTimeout 1000 $ -- FIXME timeout necessary?
        One.putQueue globalErrorQueue $ GlobalErrorConfirmEmail confEmail


  -- Global current page value - for `back` compatibility while being driven by `siteLinksSignal` -- should be read-only
  ( currentPageSignal :: IxSignal (Effects eff) siteLinks
    ) <- do
    initSiteLink <- do
      -- initial redirects
      let siteLink = initSiteLinks
          reAssign y = do
            replaceState' y h
            setDocumentTitle d $ defaultSiteLinksToDocumentTitle y
      case getUserDetailsLink siteLink of
        Just _ -> do
          -- observe preliminary auth token value immediately on boot
          case preliminaryAuthToken of
            Nothing -> do
              -- in /userDetails while not logged in
              void $ setTimeout 1000 $ -- FIXME timeouts suck ass
                One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectUserDetailsNoAuth)
              reAssign (rootLink :: siteLinks)
              pure rootLink
            _ -> pure siteLink
        _ | siteLink == registerLink -> do
          -- observe preliminary auth token value immediately on boot
          case preliminaryAuthToken of
            Just (PreliminaryAuthToken (Right _)) -> do
              -- in /register while logged in
              void $ setTimeout 1000 $ -- FIXME timeout
                One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectRegisterAuth)
              reAssign (rootLink :: siteLinks)
              pure rootLink
            _ -> pure siteLink
          | otherwise -> do
          -- observe user details value immediately, for use with extra redirects
          mUserDetails <- IxSignal.get userDetailsSignal
          case extraRedirect siteLink mUserDetails of
            Nothing -> pure siteLink
            Just y -> do
              void $ setTimeout 1000 $ -- FIXME timeout
                One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectUserDetailsNoAuth)
              reAssign y
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
          -- observe auth token value at time of onPopState
          mAuth <- IxSignal.get authTokenSignal
          case mAuth of
            Just _ -> continue siteLink
            Nothing -> do
              -- in /userDetails while not logged in
              void $ setTimeout 1000 $ -- FIXME timeouts suck ass
                One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectUserDetailsNoAuth)
              replaceState' (rootLink :: siteLinks) h
              continue rootLink
        _ | siteLink == registerLink -> do
            -- observe auth token value at time of onPopState
            mAuth <- IxSignal.get authTokenSignal
            case mAuth of
              Nothing -> continue siteLink
              Just _ -> do
                -- in /register while logged in
                void $ setTimeout 1000 $ -- FIXME timeout
                  One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectRegisterAuth)
                replaceState' (rootLink :: siteLinks) h
                continue rootLink
          | otherwise -> do
            -- observe user details value at time of onPopState, for use with extra redirects
            mUserDetails <- IxSignal.get userDetailsSignal
            case extraRedirect siteLink mUserDetails of
              Nothing -> continue siteLink
              Just y -> do
                void $ setTimeout 1000 $ -- FIXME timeout
                  One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectUserDetailsNoAuth)
                continue y

    pure sig

  -- Global new page emitter & history driver - write to this to change the page.
  ( siteLinksSignal :: One.Queue (write :: WRITE) (Effects eff) siteLinks
    ) <- do
    q <- One.newQueue
    One.onQueue q \(siteLink :: siteLinks) -> do
      -- only respect changed pages -- FIXME ??
      -- y <- IxSignal.get currentPageSignal
      -- when (y /= siteLink) $ do
      let continue x = do
            pushState' x h
            setDocumentTitle d (defaultSiteLinksToDocumentTitle x)
            IxSignal.set x currentPageSignal
      -- redirect rules
      case getUserDetailsLink siteLink of
        Just _ -> do
          -- observe auth token value at time of page change request
          mAuth <- IxSignal.get authTokenSignal
          case mAuth of
            Just _ -> continue siteLink
            Nothing -> do
              -- in /userDetails while not logged in
              void $ setTimeout 1000 $ -- FIXME timeout
                One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectUserDetailsNoAuth)
              continue rootLink
        _ | siteLink == registerLink -> do
            -- observe auth token value at time of page change request
            mAuth <- IxSignal.get authTokenSignal
            case mAuth of
              Nothing -> continue siteLink
              Just _ -> do
                -- in /register while logged in
                void $ setTimeout 1000 $ -- FIXME timeout
                  One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectRegisterAuth)
                continue rootLink
          | otherwise -> do
            -- observe user details value at time of page change request
            mUserDetails <- IxSignal.get userDetailsSignal
            case extraRedirect siteLink mUserDetails of
              Nothing -> continue siteLink
              Just y -> do
                void $ setTimeout 1000 $ -- FIXME timeout
                  One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectUserDetailsNoAuth)
                continue y
    pure (writeOnly q)


  -- rediect rules for async logout events
  let redirectOnAuth mAuth = do
        -- observe current page value at time of auth token value change
        siteLink <- IxSignal.get currentPageSignal
        let continue = do
              One.putQueue siteLinksSignal rootLink
        case getUserDetailsLink siteLink of
          Just _ -> case mAuth of
            Nothing -> do
              void $ setTimeout 1000 $ -- FIXME timeout
                One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectUserDetailsNoAuth)
              continue
            _ -> pure unit
          _ | siteLink == registerLink -> case mAuth of
              Just _ -> do
                void $ setTimeout 1000 $ -- FIXME timeout
                  One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectRegisterAuth)
                continue
              _ -> pure unit
            | otherwise -> do
              -- observe user details value at time of auth token value change
              mUserDetails <- IxSignal.get userDetailsSignal
              case extraRedirect siteLink mUserDetails of
                Nothing -> pure unit
                Just y -> do
                  void $ setTimeout 1000 $ -- FIXME timeout
                    One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectUserDetailsNoAuth)
                  One.putQueue siteLinksSignal y
  IxSignal.subscribeLight redirectOnAuth authTokenSignal

  -- auth token storage and clearing on site-wide driven changes
  let localstorageOnAuth mAuth = case mAuth of
        Nothing -> clearAuthToken
        Just authToken -> storeAuthToken authToken -- FIXME overwrites already stored value retreived on boot
  IxSignal.subscribeLight localstorageOnAuth authTokenSignal
    -- FIXME analyze the issue of having multiple auxilliary listeners on
    -- authTokenSignal changes


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
  dependenciesQueues <- newQueues newSiteQueues
  allocateDependencies (scheme == Just (Scheme "https")) authority $ do
    dependencies dependenciesQueues deps

  -- Auth Token singleton dependency mounting
  authTokenDeltaInQueue <- writeOnly <$> One.newQueue
  authTokenInitInQueue <- writeOnly <$> One.newQueue
  authTokenKillificator <- One.newQueue -- hack for killing the subscription internally, yet external for this scope

  let authTokenOnDeltaOut deltaOut = case deltaOut of
        AuthTokenDeltaOutRevoked ->
          IxSignal.set Nothing authTokenSignal
          -- TODO anything else needed to be cleaned up?
      authTokenOnInitOut mInitOut = case mInitOut of
        Nothing -> do
          IxSignal.set Nothing authTokenSignal
          One.putQueue globalErrorQueue (GlobalErrorAuthFailure AuthLoginFailure)
          One.putQueue authTokenKillificator unit
        Just initOut -> case initOut of
          AuthTokenInitOutSuccess authToken -> do
            IxSignal.set (Just authToken) authTokenSignal
          AuthTokenInitOutFailure e -> do
            IxSignal.set Nothing authTokenSignal
            One.putQueue globalErrorQueue (GlobalErrorAuthFailure e)
            One.putQueue authTokenKillificator unit

  killAuthTokenSub <- mountSparrowClientQueuesSingleton dependenciesQueues.authTokenQueues.authTokenQueues
    authTokenDeltaInQueue authTokenInitInQueue authTokenOnDeltaOut authTokenOnInitOut
  One.onQueue authTokenKillificator \_ -> killAuthTokenSub -- hack applied

  -- Top-level delta in issuer
  let authTokenDeltaIn :: AuthTokenDeltaIn -> Eff (Effects eff) Unit
      authTokenDeltaIn deltaIn = do
        One.putQueue authTokenDeltaInQueue deltaIn
        case deltaIn of
          AuthTokenDeltaInLogout -> killAuthTokenSub
          _ -> pure unit

      authTokenInitIn :: AuthTokenInitIn -> Eff (Effects eff) Unit
      authTokenInitIn = One.putQueue authTokenInitInQueue


  
  -- Handle preliminary auth token
  case preliminaryAuthToken of
    Nothing -> pure unit
    (Just (PreliminaryAuthToken eErr)) -> case eErr of
      Right prescribedAuthToken ->
        authTokenInitIn (AuthTokenInitInExists prescribedAuthToken)
      Left e -> -- FIXME ...needs timeout?
        One.putQueue globalErrorQueue $ GlobalErrorAuthFailure e

  
  -- Dialog queues
  ( loginCloseQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
    ) <- writeOnly <$> One.newQueue

  -- user details fetcher and clearer
  let userDetailsOnAuth mAuth = case mAuth of
        Nothing -> IxSignal.set Nothing userDetailsSignal
        Just authToken -> do
          let resolve eX = case eX of
                Left _ -> do
                  IxSignal.set Nothing userDetailsSignal
                  One.putQueue globalErrorQueue (GlobalErrorUserEmail UserEmailNoInitOut)
                Right mUserDetails -> do
                  IxSignal.set mUserDetails userDetailsSignal
                  One.putQueue loginCloseQueue unit -- FIXME user details only obtained from login?? Idempotent?

          -- Utilize userDetail's obtain method
          runAff_ resolve $ userDetails.obtain
            { user: parallel $ do
                -- FIXME use getuser stuff
                mInitOut <- OneIO.callAsync dependenciesQueues.commonQueues.getUserQueues
                  (AccessInitIn {token: authToken, subj: JSONUnit})
                case mInitOut of
                  Nothing -> do
                    liftEff (One.putQueue globalErrorQueue (GlobalErrorUserEmail UserEmailNoInitOut))
                    pure Nothing
                  Just user -> pure (Just user)
            }
  IxSignal.subscribeLight userDetailsOnAuth authTokenSignal


  -- universal React component params
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
          { env
          , globalErrorQueue
          , dialogQueues:
            { login:
              { closeQueue: loginCloseQueue
              }
            }
          , dependenciesQueues
          , authTokenInitIn
          , authTokenDeltaIn
          , templateArgs:
            { content
            , topbar
            , leftDrawer
            , userDetails:
              { buttons: userDetails.buttons
              , content: userDetails.content
              }
            , palette
            , extendedNetwork
            , register:
              { unsavedFormDataQueue: registerUnsavedFormData
              }
            , security:
              { unsavedFormDataQueue: securityUnsavedFormData
              }
            }
          }
      component = R.createClass reactSpec
  traverse_ (render (R.createFactory component props) <<< htmlElementToElement) =<< body d
