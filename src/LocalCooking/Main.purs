module LocalCooking.Main where

import LocalCooking.Spec (app)
import LocalCooking.Spec.Content.Register (RegisterUnsavedFormData (..))
import LocalCooking.Spec.Content.UserDetails.Security (SecurityUnsavedFormData (..))
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Types.ServerToClient (ServerToClient (..), serverToClient)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Auth.Storage (getStoredAuthToken, storeAuthToken, clearAuthToken)
import LocalCooking.Global.Error
  (GlobalError (..), UserEmailError (..), AuthTokenFailure (..), SecurityMessage (..), RedirectError (RedirectLogout))
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks, rootLink, pushState', replaceState', onPopState, defaultSiteLinksToDocumentTitle, initSiteLinks, withRedirectPolicy)
import LocalCooking.Global.User.Class (class UserDetails)
import LocalCooking.Dependencies (dependencies, newQueues)
import LocalCooking.Dependencies.AuthToken (PreliminaryAuthToken (..), AuthTokenDeltaOut (..), AuthTokenInitOut (..), AuthTokenDeltaIn (..), AuthTokenInitIn (..))
import LocalCooking.Dependencies.Common (UserDeltaOut (..), UserInitOut (..), UserDeltaIn, UserInitIn (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Semantics.Common (User)
import Facebook.State (FacebookLoginUnsavedFormData (..))

import Sparrow.Client (allocateDependencies)
import Sparrow.Client.Queue (mountSparrowClientQueuesSingleton)
import Sparrow.Client.Types (SparrowClientT)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.URI (Authority (..), Host (NameAddress), Scheme (..), Port (..))
import Data.URI.Location (Location, toURI, class ToLocation, class FromLocation)
import Data.URI.URI (URI)
import Data.String (takeWhile) as String
import Data.Int.Parse (parseInt, toRadix)
import Data.UUID (GENUUID)
import Data.Traversable (traverse_)
import Data.Time.Duration (Milliseconds (..))
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Data.Argonaut.JSONTuple (JSONTuple (..))
import Data.Generic (class Generic)
import Control.Monad.Aff (ParAff, Aff, runAff_, parallel)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Execution.Immediate (SET_IMMEDIATE_SHIM, registerShim)

import React (ReactElement)
import React as R
import ReactDOM (render)
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT, injectTapEvent)
import MaterialUI.MuiThemeProvider (ColorPalette)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location, document, history)
import DOM.HTML.Window.Extra (widthToWindowSize)
import DOM.HTML.History (back)
import DOM.HTML.Location (hostname, protocol, port)
import DOM.HTML.Document (body)
import DOM.HTML.Document.Extra (setDocumentTitle)
import DOM.HTML.Types (HISTORY, htmlElementToElement, Window, HTMLDocument, History)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal (get, make, set, setDiff, subscribeLight, subscribeDiffLight) as IxSignal
import IxSignal.Extra (onNext) as IxSignal
import Signal.Internal as Signal
import Signal.Time (debounce)
import Signal.DOM (windowDimensions)
import Queue.Types (writeOnly, readOnly)
import Queue (READ, WRITE)
import Queue.One as One
import Browser.WebStorage (WEB_STORAGE)
import WebSocket (WEBSOCKET)
import Network.HTTP.Affjax (AJAX)
import Crypto.Scrypt (SCRYPT)



foreign import clearBody :: forall eff. Eff (dom :: DOM | eff) Unit


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


-- | Restricted form of LocalCookingParams
type ExtraProcessingParams siteLinks userDetails eff =
  { siteLinks         :: siteLinks -> Eff eff Unit
  , back              :: Eff eff Unit
  , toURI             :: Location -> URI
  , authTokenSignal   :: IxSignal eff (Maybe AuthToken)
  , userDetailsSignal :: IxSignal eff (Maybe userDetails)
  }


type LocalCookingArgs siteLinks userDetails siteError eff =
  { content :: LocalCookingParams siteLinks userDetails eff -> ReactElement -- ^ Primary content process
  , topbar ::
    { imageSrc :: Location -- ^ Nify colored logo
    , buttons :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement -> ReactElement
    }
  , leftDrawer ::
    { buttons :: LocalCookingParams siteLinks userDetails eff -> ReactElement -> ReactElement -- ^ Mobile only
    }
  , userDetails ::
    { buttons :: LocalCookingParams siteLinks userDetails eff -> Array ReactElement -> ReactElement -> ReactElement -- ^ Side navigation
    , content :: LocalCookingParams siteLinks userDetails eff -> ReactElement
    , obtain  :: -- ^ Given a method to obtain a few fields, obtain the entire struct
      { user  :: ParAff eff (Maybe User)
      } -> Aff eff (Maybe userDetails)
    }
  , error ::
    { content ::
      { siteErrorQueue :: One.Queue (read :: READ) eff siteError
      } -> ReactElement
    , queue :: One.Queue (read :: READ, write :: WRITE) eff siteError
    }
  , deps          :: SparrowClientT eff (Eff eff) Unit -- ^ Apply those queues -- FIXME TODO MonadBaseControl?
  , extraRedirect :: siteLinks
                  -> Maybe userDetails
                  -> Maybe
                     { siteLink :: siteLinks
                     , siteError :: siteError
                     } -- ^ Additional redirection rules per-site
  , extraProcessing :: siteLinks -> ExtraProcessingParams siteLinks userDetails eff -> Eff eff Unit
  , initToDocumentTitle :: siteLinks -> String -- ^ Get prefix for initial state
  , asyncToDocumentTitle :: siteLinks -> Aff eff String -- ^ Get prefix effectfully
  , palette :: -- ^ Colors
    { primary   :: ColorPalette
    , secondary :: ColorPalette
    }
  , extendedNetwork :: Array R.ReactElement -- ^ Buttons
  , env :: Env -- ^ Provided by Server.FrontendEnv
  }



-- | Top-level entry point to the application
defaultMain :: forall eff siteLinks userDetailsLinks userDetails siteError
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => Eq siteLinks
            => Eq userDetails
            => Show siteLinks
            => Show userDetails
            => Generic siteLinks
            => Generic userDetails
            => ToLocation siteLinks
            => FromLocation siteLinks
            => UserDetails userDetails
            => LocalCookingArgs siteLinks userDetails siteError (Effects eff)
            -> Eff (Effects eff) Unit
defaultMain
  { deps
  , topbar
  , leftDrawer
  , content
  , userDetails
  , env
  , palette
  , extendedNetwork
  , extraRedirect
  , extraProcessing
  , initToDocumentTitle
  , asyncToDocumentTitle
  , error
  } = do
  -- clear loader and SEO content
  clearBody

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


  -- Spit out confirm email message if it exists
  case serverToClient of
    ServerToClient {confirmEmail} -> case confirmEmail of
      Nothing -> pure unit
      Just confEmail -> void $ setTimeout 1000 $ -- FIXME timeout necessary?
        One.putQueue globalErrorQueue $ GlobalErrorConfirmEmail confEmail


  preliminarySiteLinksQueue <- One.newQueue
  let preliminaryParams =
        { siteLinks: One.putQueue preliminarySiteLinksQueue
        , back: back h
        , toURI: \location -> toURI {scheme, authority: Just authority, location}
        , authTokenSignal
        , userDetailsSignal
        }


  -- Global current page value - for `back` compatibility while being driven by `siteLinksQueue` -- should be read-only
  ( currentPageSignal :: IxSignal (Effects eff) siteLinks
    ) <-
    mkCurrentPageSignal
      { w,h,d
      , authTokenSignal
      , userDetailsSignal
      , initToDocumentTitle
      , asyncToDocumentTitle
      , extraProcessing
      , extraRedirect
      , preliminaryParams
      , globalErrorQueue: writeOnly globalErrorQueue
      , error
      }

  -- Global new page emitter & history driver - write to this to change the page.
  ( siteLinksQueue :: One.Queue (write :: WRITE) (Effects eff) siteLinks
    ) <- do
    q <- One.newQueue
    One.onQueue q \(siteLink :: siteLinks) -> do
      let continue x = do
            let resolveEffectiveDocumentTitle eX = case eX of
                  Left e -> warn $ "Couldn't resolve asyncToDocumentTitle in siteLinksQueue: " <> show e
                  Right pfx -> do
                    pushState' pfx x h
                    setDocumentTitle d (defaultSiteLinksToDocumentTitle pfx x)
            runAff_ resolveEffectiveDocumentTitle (asyncToDocumentTitle x)
            IxSignal.setDiff x currentPageSignal
            extraProcessing x preliminaryParams
      authToken <- IxSignal.get authTokenSignal
      userDetails <- IxSignal.get userDetailsSignal
      y <- withRedirectPolicy
        { onError: pure unit
        , extraRedirect
        , authToken
        , userDetails
        , globalErrorQueue: writeOnly globalErrorQueue
        , siteErrorQueue: writeOnly error.queue
        }
        siteLink
      when (y /= siteLink) $
        warn $ "Not driving siteLink to intended target - intended: " <> show siteLink <> ", actual: " <> show y
      continue y
    pure (writeOnly q)

  One.onQueue preliminarySiteLinksQueue (One.putQueue siteLinksQueue)

  hasDrawnFromUserDetails <- newRef false

  -- rediect rules for async logout events
  let redirectOnAuth mAuth = do
        -- observe current page value at time of auth token value change
        siteLink <- IxSignal.get currentPageSignal
        drawn <- readRef hasDrawnFromUserDetails
        writeRef hasDrawnFromUserDetails true
        let process userDetails = do
              let continue link = do
                    One.putQueue siteLinksQueue link
                    extraProcessing link preliminaryParams
              y <- withRedirectPolicy
                { onError: pure unit
                , extraRedirect
                , authToken: mAuth
                , userDetails
                , globalErrorQueue: writeOnly globalErrorQueue
                , siteErrorQueue: writeOnly error.queue
                }
                siteLink
              when (y /= siteLink) $ do
                log $ "Redirecting due to auth signal change - old: " <> show siteLink <> ", new: " <> show y <> ", user details: " <> show userDetails
                continue y
        if drawn
           then process =<< IxSignal.get userDetailsSignal
           else IxSignal.onNext process userDetailsSignal
  IxSignal.subscribeLight redirectOnAuth authTokenSignal

  -- auth token storage and clearing on site-wide driven changes
  let localstorageOnAuth mAuth = case mAuth of
        Nothing -> clearAuthToken
        Just authToken -> storeAuthToken authToken -- FIXME overwrites already stored value retreived on boot
  IxSignal.subscribeDiffLight localstorageOnAuth authTokenSignal


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
        IxSignal.setDiff size out
    pure out


  -- Dialog queues
  ( loginCloseQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
    ) <- writeOnly <$> One.newQueue


  -- Sparrow dependencies ------------------------------------------------------
  dependenciesQueues <- newQueues
  allocateDependencies (scheme == Just (Scheme "https")) authority $ do
    dependencies dependenciesQueues deps

  -- Auth Token singleton dependency mounting
  authTokenDeltaInQueue <- writeOnly <$> One.newQueue
  authTokenInitInQueue <- writeOnly <$> One.newQueue
  authTokenKillificator <- One.newQueue -- hack for killing the subscription internally, yet external for this scope

  let authTokenOnDeltaOut deltaOut = case deltaOut of
        AuthTokenDeltaOutRevoked -> do
          log "Auth token revoked"
          IxSignal.setDiff Nothing authTokenSignal
          -- TODO anything else needed to be cleaned up?
      authTokenOnInitOut mInitOut = case mInitOut of
        Nothing -> do
          log "Auth login failure"
          IxSignal.setDiff Nothing authTokenSignal
          One.putQueue globalErrorQueue (GlobalErrorAuthFailure AuthLoginFailure)
          One.putQueue authTokenKillificator unit
        Just initOut -> case initOut of
          AuthTokenInitOutSuccess authToken -> do
            log $ "Auth login success: " <> show authToken
            IxSignal.set (Just authToken) authTokenSignal
          AuthTokenInitOutFailure e -> do
            log $ "Auth login failure: " <> show e
            IxSignal.set Nothing authTokenSignal
            One.putQueue globalErrorQueue (GlobalErrorAuthFailure e)
            One.putQueue authTokenKillificator unit

  killAuthTokenSub <- mountSparrowClientQueuesSingleton dependenciesQueues.authTokenQueues.authTokenQueues
    authTokenDeltaInQueue authTokenInitInQueue authTokenOnDeltaOut authTokenOnInitOut
  One.onQueue authTokenKillificator \_ -> do
    killAuthTokenSub -- hack applied

  -- Top-level delta in issuer
  let authTokenDeltaIn :: AuthTokenDeltaIn -> Eff (Effects eff) Unit
      authTokenDeltaIn deltaIn = do
        log "Sending auth delta in"
        One.putQueue authTokenDeltaInQueue deltaIn
        case deltaIn of
          AuthTokenDeltaInLogout -> do
            One.putQueue siteLinksQueue rootLink
            One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectLogout)
            IxSignal.setDiff Nothing authTokenSignal
            One.putQueue authTokenKillificator unit

      authTokenInitIn :: AuthTokenInitIn -> Eff (Effects eff) Unit
      authTokenInitIn initIn = do
        log "Sending auth init in"
        One.putQueue authTokenInitInQueue initIn

  -- Auth Token singleton dependency mounting
  userDeltaInQueue <- writeOnly <$> One.newQueue
  userInitInQueue <- writeOnly <$> One.newQueue
  -- userKillificator <- One.newQueue -- hack for killing the subscription internally, yet external for this scope

  let userOnDeltaOut deltaOut = case deltaOut of
        UserDeltaOutSetUserFailure ->
          One.putQueue globalErrorQueue (GlobalErrorSecurity SecuritySaveFailed)
        UserDeltaOutSetUserSuccess ->
          One.putQueue globalErrorQueue (GlobalErrorSecurity SecuritySaveSuccess)
        _ -> pure unit
      userOnInitOut mInitOut = do
        let resolve eX = do
              case eX of
                Left e -> do
                  log $ "User details obtain failure: " <> show e
                  IxSignal.setDiff Nothing userDetailsSignal
                  One.putQueue globalErrorQueue (GlobalErrorUserEmail UserEmailNoInitOut)
                Right mUserDetails -> do
                  log $ "User details obtain success: " <> show mUserDetails
                  IxSignal.setDiff mUserDetails userDetailsSignal
                  One.putQueue loginCloseQueue unit -- FIXME user details only obtained from login?? Idempotent?
        runAff_ resolve $
          case mInitOut of
            Nothing ->
              userDetails.obtain
                { user: parallel $ pure Nothing
                }
            Just (UserInitOut user) -> do -- FIXME OBTAIN
              userDetails.obtain
                { user: parallel $ pure $ Just user -- do
                }

  _ <- mountSparrowClientQueuesSingleton dependenciesQueues.commonQueues.userQueues
    userDeltaInQueue userInitInQueue userOnDeltaOut userOnInitOut
  -- One.onQueue userKillificator \_ -> killUserSub -- hack applied

  -- Top-level delta in issuer
  let userDeltaIn :: UserDeltaIn -> Eff (Effects eff) Unit
      userDeltaIn = One.putQueue userDeltaInQueue

      userInitIn :: UserInitIn -> Eff (Effects eff) Unit
      userInitIn = One.putQueue userInitInQueue



  -- Handle preliminary auth token
  case preliminaryAuthToken of
    Nothing -> log "no preliminary token"
    (Just (PreliminaryAuthToken eErr)) -> case eErr of
      Right prescribedAuthToken ->
        authTokenInitIn (AuthTokenInitInExists prescribedAuthToken)
      Left e -> do -- FIXME ...needs timeout?
        One.putQueue globalErrorQueue $ GlobalErrorAuthFailure e
        -- try and recover even during weird init error
        mTkn <- getStoredAuthToken
        case mTkn of
          Nothing -> log "no stored token either"
          Just storedAuthToken ->
            authTokenInitIn (AuthTokenInitInExists storedAuthToken)



  -- user details fetcher and clearer
  let userDetailsOnAuth mAuth = do
        log $ "fetching user deets: " <> show mAuth
        case mAuth of
          Nothing -> IxSignal.set Nothing userDetailsSignal
          Just authToken -> userInitIn $ UserInitIn $ JSONTuple authToken JSONUnit
  IxSignal.subscribeLight userDetailsOnAuth authTokenSignal


  -- universal React component params
  let params :: LocalCookingParams siteLinks userDetails (Effects eff)
      params =
        { toURI : \location -> toURI {scheme, authority: Just authority, location}
        , siteLinks : One.putQueue siteLinksQueue
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
          , userInitIn
          , userDeltaIn
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
            , error:
              { content: error.content {siteErrorQueue: readOnly error.queue}
              }
            }
          }
      component = R.createClass reactSpec
  traverse_ (render (R.createFactory component props) <<< htmlElementToElement) =<< body d




-- | Global current page value - for `back` compatibility while being driven by `siteLinksQueue` -- should be read-only
mkCurrentPageSignal :: forall eff siteLinks userDetails userDetailsLinks siteError
                     . Eq siteLinks
                    => Show siteLinks
                    => Show userDetails
                    => ToLocation siteLinks
                    => FromLocation siteLinks
                    => LocalCookingSiteLinks siteLinks userDetailsLinks
                    => { w :: Window
                       , h :: History
                       , d :: HTMLDocument
                       , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                       , userDetailsSignal :: IxSignal (Effects eff) (Maybe userDetails)
                       , initToDocumentTitle :: siteLinks -> String
                       , asyncToDocumentTitle :: siteLinks -> Aff (Effects eff) String
                       , extraRedirect :: siteLinks
                                       -> Maybe userDetails
                                       -> Maybe
                                          { siteLink :: siteLinks
                                          , siteError :: siteError
                                          } -- ^ Additional redirection rules per-site
                       , extraProcessing :: siteLinks
                                         -- restricted form of LocalCookingParams
                                         -> { siteLinks         :: siteLinks -> Eff (Effects eff) Unit
                                            , back              :: Eff (Effects eff) Unit
                                            , toURI             :: Location -> URI
                                            , authTokenSignal   :: IxSignal (Effects eff) (Maybe AuthToken)
                                            , userDetailsSignal :: IxSignal (Effects eff) (Maybe userDetails)
                                            } -> Eff (Effects eff) Unit
                       , preliminaryParams ::
                         { siteLinks :: siteLinks -> Eff (Effects eff) Unit
                         , back :: Eff (Effects eff) Unit
                         , toURI :: Location -> URI
                         , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                         , userDetailsSignal :: IxSignal (Effects eff) (Maybe userDetails)
                         }
                       , globalErrorQueue :: One.Queue (write :: WRITE) (Effects eff) GlobalError
                       , error ::
                         { content ::
                           { siteErrorQueue :: One.Queue (read :: READ) (Effects eff) siteError
                           } -> ReactElement
                         , queue :: One.Queue (read :: READ, write :: WRITE) (Effects eff) siteError
                         }
                       }
                    -> Eff (Effects eff) (IxSignal (Effects eff) siteLinks)
mkCurrentPageSignal
  { w,h,d
  , authTokenSignal
  , userDetailsSignal
  , initToDocumentTitle
  , asyncToDocumentTitle
  , extraProcessing
  , extraRedirect
  , preliminaryParams
  , globalErrorQueue
  , error
  } = do
  let reAssign pfx y = do
        log $ "ReAssigning parsed siteLink, within currentPageSignal definition: " <> show y
        replaceState' pfx y h

  initSiteLink <- do
    -- initial site link, and redirects after user details loads
    (siteLink :: siteLinks) <- initSiteLinks
    authToken <- IxSignal.get authTokenSignal

    let initPfx = initToDocumentTitle siteLink

    reAssign initPfx siteLink
    setDocumentTitle d (defaultSiteLinksToDocumentTitle initPfx siteLink)

    let resolveEffectiveInitDocumentTitle eX = case eX of
          Left e -> warn $ "Couldn't resolve asyncToDocumentTitle in initSiteLinks: " <> show e
          Right pfx -> setDocumentTitle d (defaultSiteLinksToDocumentTitle pfx siteLink)
    runAff_ resolveEffectiveInitDocumentTitle (asyncToDocumentTitle siteLink)
    extraProcessing siteLink preliminaryParams

    pure siteLink

  sig <- IxSignal.make initSiteLink

  -- fetch user details' first value asynchronously, compensate for possibly erroneous
  -- initial site link, and handle the first possible redirect genuinely
  let resolveUserDetails userDetails = do
        authToken <- IxSignal.get authTokenSignal
        z <- withRedirectPolicy
          { onError: pure unit
          , extraRedirect
          , authToken
          , userDetails
          , globalErrorQueue: writeOnly globalErrorQueue
          , siteErrorQueue: writeOnly error.queue
          }
          initSiteLink
        when (z /= initSiteLink) $ do
          log $ "Redirecting after user details loaded - old: " <> show initSiteLink <> ", new: " <> show z <> ", user details: " <> show userDetails
          let resolveEffectiveDocumentTitle eX = case eX of
                Left e -> warn $ "Couldn't get effective document title after user details load: " <> show e
                Right pfx -> do
                  reAssign pfx z
                  setDocumentTitle d (defaultSiteLinksToDocumentTitle pfx z)
                  extraProcessing z preliminaryParams
          runAff_ resolveEffectiveDocumentTitle (asyncToDocumentTitle z)
  IxSignal.onNext resolveUserDetails userDetailsSignal

  

  -- handle back & forward
  flip onPopState w \(siteLink :: siteLinks) -> do
    log $ "Received onPopState link: " <> show siteLink
    let continue x = do
          let resolveEffectiveDocumentTitle eX = case eX of
                Left e -> warn $ "Couldn't resolve asyncToDocumentTitle in onPopState: " <> show e
                Right pfx -> setDocumentTitle d (defaultSiteLinksToDocumentTitle pfx x)
          runAff_ resolveEffectiveDocumentTitle (asyncToDocumentTitle x)
          IxSignal.setDiff x sig
          extraProcessing x preliminaryParams
    authToken <- IxSignal.get authTokenSignal
    userDetails <- IxSignal.get userDetailsSignal
    y <- withRedirectPolicy
      { onError: replaceState' (initToDocumentTitle (rootLink :: siteLinks)) (rootLink :: siteLinks) h
                  -- okay to rely on initToDocumentTitle, because rootLink is guaranteed non-effective prefix
      , extraRedirect
      , authToken
      , userDetails
      , globalErrorQueue: writeOnly globalErrorQueue
      , siteErrorQueue: writeOnly error.queue
      }
      siteLink
    continue y

  pure sig




replicateM_ :: forall m. Monad m => Int -> m Unit -> m Unit
replicateM_ n x
  | n == 0 = pure unit
  | otherwise = x *> replicateM_ (n - 1) x
