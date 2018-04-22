module LocalCooking.Spec.Dialogs.PrivacyPolicy where

import LocalCooking.Spec.Content.Register.Pending (pending)
import LocalCooking.Spec.Form.Email (email)
import LocalCooking.Spec.Form.Password (password)
import LocalCooking.Spec.Form.Submit (submit)
import LocalCooking.Types.Env (Env)
import LocalCooking.Window (WindowSize (..))
import LocalCooking.Links (PolicyLinks (..))
import LocalCooking.Links.Class (registerLink, toLocation, class LocalCookingSiteLinks, class ToLocation)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (genUUID, GENUUID)
import Data.Time.Duration (Milliseconds (..))
import Text.Email.Validate (EmailAddress)
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)
import React.Queue.WhileMounted as Queue
import React.Signal.WhileMounted as Signal
import React.Icons (facebookIcon, twitterIcon, googleIcon)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)

import MaterialUI.Types (createStyles)
import MaterialUI.Dialog (dialog)
import MaterialUI.DialogContent (dialogContent)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import Crypto.Scrypt (SCRYPT)

import Queue.One (READ, Queue)
import IxQueue (IxQueue)
import IxQueue as IxQueue
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State siteLinks =
  { open        :: Boolean
  , windowSize  :: WindowSize
  , currentPage :: siteLinks
  }


initialState :: forall siteLinks
              . {initSiteLinks :: siteLinks, initWindowSize :: WindowSize} -> State siteLinks
initialState {initWindowSize, initSiteLinks} =
  { open: false
  , windowSize: initWindowSize
  , currentPage: initSiteLinks
  }


data Action siteLinks
  = Open
  | Close
  | ChangedWindowSize WindowSize
  | ChangedPage siteLinks
  | SubmitPrivacyPolicy

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  , console   :: CONSOLE
  , dom       :: DOM
  , history   :: HISTORY
  | eff)


spec :: forall eff siteLinks userDetailsLinks
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => ToLocation siteLinks
     => { privacyPolicySignal :: IxSignal (Effects eff) Boolean
        , toURI             :: Location -> URI
        }
     -> T.Spec (Effects eff) (State siteLinks) Unit (Action siteLinks)
spec
  { privacyPolicySignal
  , toURI
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close ->
        void $ T.cotransform _ { open = false }
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedPage p -> void $ T.cotransform _ { currentPage = p }
      SubmitPrivacyPolicy -> do
        liftEff $ IxSignal.set false privacyPolicySignal
        performAction Close props state

    render :: T.Render (State siteLinks) Unit (Action siteLinks)
    render dispatch props state children =
      [ let dialog' =
              if state.windowSize < Laptop
              then
                dialog
                  { open: state.open
                  , fullScreen: true
                  }
              else
                dialog
                  { open: state.open
                  , fullWidth: true
                  , onClose: mkEffFn1 \_ -> dispatch Close
                  }
        in  dialog'
            [ dialogTitle {} [R.text "Privacy Policy"]
            , dialogContent {style: createStyles {position: "relative"}}
              [ R.iframe
                [ RP.src $ URI.print $ toURI $ toLocation PrivacyPolicyLink
                , RP.style {width: "100%", border: "1px solid black"}
                ] []
              ]
            , dialogActions {}
              [ button
                { color: Button.primary
                , onTouchTap: mkEffFn1 \_ -> dispatch SubmitPrivacyPolicy
                } [R.text "Acknowledge"]
              ]
            ]
      ]



privacyPolicyDialog :: forall eff siteLinks userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => ToLocation siteLinks
            => { privacyPolicySignal :: IxSignal (Effects eff) Boolean
               , windowSizeSignal    :: IxSignal (Effects eff) WindowSize
               , currentPageSignal   :: IxSignal (Effects eff) siteLinks
               , toURI               :: Location -> URI
               }
            -> R.ReactElement
privacyPolicyDialog
  { privacyPolicySignal
  , windowSizeSignal
  , currentPageSignal
  , toURI
  } =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        , initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { toURI
            , privacyPolicySignal
            } )
          (initialState init)
      reactSpecPrivacyPolicy =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedPage x))
        $ Signal.whileMountedIxUUID
            privacyPolicySignal
            (\this p -> unsafeCoerceEff $ dispatcher this (if p then Open else Close))
            reactSpec
  in  R.createElement (R.createClass reactSpecPrivacyPolicy) unit []
