module LocalCooking.Spec.Dialogs.Generic where

import LocalCooking.Spec.Form.Pending (pending)
import LocalCooking.Spec.Form.Submit as Submit
import LocalCooking.Spec.Snackbar (SnackbarMessage)
import LocalCooking.Types.Env (Env)
import LocalCooking.Types.Params (LocalCookingParams, LocalCookingAction, LocalCookingState, performActionLocalCooking, whileMountedLocalCooking, initLocalCookingState)
import LocalCooking.Window (WindowSize (..))
import LocalCooking.Links.Class (class LocalCookingSiteLinks, class ToLocation)

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.Time.Duration (Milliseconds (..))
import Data.Lens (Lens', Prism', lens, prism')
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
import React as R
import React.DOM as R
import React.Queue.WhileMounted as Queue
import DOM (DOM)

import MaterialUI.Types (createStyles)
import MaterialUI.Dialog (dialog)
import MaterialUI.DialogContent (dialogContent)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Button (button)
import MaterialUI.Button as Button

import Queue.Types (readOnly, allowReading)
import Queue (READ, WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State siteLinks userDetails =
  { open         :: Boolean
  , localCooking :: LocalCookingState siteLinks userDetails
  }


initialState :: forall siteLinks userDetails
              . LocalCookingState siteLinks userDetails -> State siteLinks userDetails
initialState localCooking =
  { open: false
  , localCooking
  }


data Action siteLinks userDetails
  = Open
  | Close
  | Submit
  | LocalCookingAction (LocalCookingAction siteLinks userDetails)

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , dom       :: DOM
  | eff)


getLCState :: forall siteLinks userDetails
            . Lens' (State siteLinks userDetails) (LocalCookingState siteLinks userDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })

getLCAction :: forall siteLinks userDetails
             . Prism' (Action siteLinks userDetails) (LocalCookingAction siteLinks userDetails)
getLCAction = prism' LocalCookingAction $ case _ of
  LocalCookingAction x -> Just x
  _ -> Nothing


spec :: forall eff siteLinks userDetails userDetailsLinks output
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => ToLocation siteLinks
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { env :: Env
        , dialogOutputQueue :: One.Queue (write :: WRITE) (Effects eff) (Maybe output)
        , errorMessageQueue :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
        , closeQueue :: Maybe (One.Queue (write :: WRITE) (Effects eff) Unit)
        , content ::
          { component ::
            { submitDisabled :: Boolean -> Eff (Effects eff) Unit
            } -> Array R.ReactElement
          , obtain    :: Aff (Effects eff) (Maybe output)
          , reset     :: Eff (Effects eff) Unit
          }
        , buttons ::
          { close :: Eff (Effects eff) Unit
          } -> Array R.ReactElement
        , title :: String
        , submit ::
          { disabledSignal :: IxSignal (Effects eff) Boolean
          , queue          :: IxQueue (read :: READ) (Effects eff) Unit
          , value          :: String
          }
        , pendingSignal :: Maybe (IxSignal (Effects eff) Boolean)
        }
     -> T.Spec (Effects eff) (State siteLinks userDetails) Unit (Action siteLinks userDetails)
spec
  {toURI}
  { env
  , submit
  , content
  , pendingSignal
  , dialogOutputQueue
  , closeQueue
  , buttons
  , title
  , errorMessageQueue
  } = T.simpleSpec (performAction <> performActionLocalCooking getLCState getLCAction) render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close -> do
        case pendingSignal of
          Nothing -> pure unit
          Just p  -> liftEff (IxSignal.set false p)
        void $ T.cotransform _ { open = false }
        liftBase $ delay $ Milliseconds 2000.0
        liftEff content.reset
      Submit -> do
        case pendingSignal of
          Nothing -> pure unit
          Just p -> liftEff (IxSignal.set true p)
        mOutput <- liftBase content.obtain
        case mOutput of
          Nothing -> pure unit -- FIXME error out?
          Just output -> do
            case closeQueue of
              Nothing -> performAction Close props state
              Just closeQueue' -> pure unit
            liftEff (One.putQueue dialogOutputQueue (Just output))
      _ -> pure unit

    render :: T.Render (State siteLinks userDetails) Unit (Action siteLinks userDetails)
    render dispatch props state children =
      [ let dialog' =
              if state.localCooking.windowSize < Laptop
              then
                dialog
                  { open: state.open
                  , fullScreen: true
                  }
              else
                dialog
                  { open: state.open
                  , fullWidth: true
                  , onClose: mkEffFn1 \_ -> do
                      pending <- do
                        case pendingSignal of
                          Nothing -> pure false
                          Just p  -> unsafeCoerceEff (IxSignal.get p)
                      when (not pending) $ do
                        unsafeCoerceEff (One.putQueue dialogOutputQueue Nothing)
                        dispatch Close
                  }
        in  dialog'
            [ dialogTitle {} [R.text title]
            , dialogContent {style: createStyles {position: "relative"}} $
                content.component
                { submitDisabled: \d -> IxSignal.set d submit.disabledSignal
                } <>
                  case pendingSignal of
                    Nothing -> []
                    Just p  ->
                      [ pending
                        { pendingSignal: p
                        }
                      ]
            , dialogActions {} $
                buttons
                { close: do
                    unsafeCoerceEff (dispatch Close)
                    One.putQueue dialogOutputQueue Nothing
                } <>
                  [ Submit.submit
                    { color: Button.primary
                    , variant: Button.flat
                    , size: Button.medium
                    , style: createStyles {}
                    , triggerQueue: submit.queue
                    , disabledSignal: submit.disabledSignal
                    } [R.text submit.value]
                  , button
                    { color: Button.default
                    , onTouchTap: mkEffFn1 \_ -> do
                        unsafeCoerceEff (One.putQueue dialogOutputQueue Nothing)
                        dispatch Close
                    } [R.text "Cancel"]
                  ]
            ]
      ]



genericDialog :: forall eff siteLinks userDetails userDetailsLinks output
               . LocalCookingSiteLinks siteLinks userDetailsLinks
              => ToLocation siteLinks
              => LocalCookingParams siteLinks userDetails (Effects eff)
              -> { dialogQueue       :: OneIO.IOQueues (Effects eff) Unit (Maybe output)
                 , errorMessageQueue :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
                 , closeQueue        :: Maybe (One.Queue (write :: WRITE) (Effects eff) Unit)
                 , env               :: Env
                 , buttons           ::
                    { close :: Eff (Effects eff) Unit
                    } -> Array R.ReactElement
                 , title             :: String
                 , submitValue       :: String
                 , pends             :: Boolean
                 , content ::
                   { component ::
                     { submitDisabled :: Boolean -> Eff (Effects eff) Unit
                     } -> Array R.ReactElement
                   , obtain    :: Aff (Effects eff) (Maybe output)
                   , reset     :: Eff (Effects eff) Unit
                   }
                 }
              -> R.ReactElement
genericDialog
  params
  { dialogQueue: OneIO.IOQueues {input: dialogInputQueue, output: dialogOutputQueue}
  , errorMessageQueue
  , closeQueue
  , env
  , content
  , submitValue
  , buttons
  , title
  , pends
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { env
            , errorMessageQueue
            , closeQueue
            , buttons
            , title
            , submit:
              { queue: submitQueue
              , disabledSignal: submitDisabledSignal
              , value: submitValue
              }
            , content
            , pendingSignal
            , dialogOutputQueue
            } )
          (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            getLCAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
        $ Queue.whileMountedOne
            dialogInputQueue
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
        $ ( case closeQueue of
              Nothing -> id
              Just closeQueue' ->
                Queue.whileMountedOne
                  (allowReading closeQueue')
                  (\this _ -> unsafeCoerceEff $ dispatcher this Close)
          )
        $ Queue.whileMountedIxUUID
            submitQueue
            (\this _ -> unsafeCoerceEff $ dispatcher this Submit)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    submitDisabledSignal = unsafePerformEff $ IxSignal.make false
    pendingSignal = if pends then unsafePerformEff (Just <$> IxSignal.make false) else Nothing
    submitQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
