module LocalCooking.Spec.Form.Email where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Text.Email.Validate (EmailAddress, emailAddress)
import Text.Email.Validate as Email
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Console (log)

import Thermite as T
import React as R
import React.DOM as R
import React.Queue.WhileMounted as Queue

import MaterialUI.TextField (textField)
import MaterialUI.Input as Input

import Unsafe.Coerce (unsafeCoerce)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue (READ, WRITE)
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue



type State =
  { email :: String
  , rerender :: Unit
  }

initialState :: {initEmail :: String} -> State
initialState {initEmail} =
  { email: initEmail
  , rerender: unit
  }

data Action
  = ChangedEmail String
  | EmailUnfocused
  | ReRender
  | GotExternalValue String

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff
      . { emailSignal  :: IxSignal (Effects eff) (Either String (Maybe EmailAddress))
        , parentSignal :: Maybe (IxSignal (Effects eff) (Either String (Maybe EmailAddress)))
        , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
        , label        :: R.ReactElement
        , fullWidth    :: Boolean
        , name         :: String
        , id           :: String
        } -> T.Spec (Effects eff) State Unit Action
spec
  { emailSignal
  , parentSignal
  , updatedQueue
  , label
  , fullWidth
  , name
  , id
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedEmail e -> do
        liftEff $ IxSignal.set (Left e) emailSignal
        void $ T.cotransform _ { email = e }
      EmailUnfocused -> do
        liftEff $ case emailAddress state.email of
          Nothing -> IxSignal.set (Right Nothing) emailSignal
          Just e -> IxSignal.set (Right (Just e)) emailSignal
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (IxQueue.allowWriting updatedQueue) unit
      ReRender -> void $ T.cotransform _ { rerender = unit }
      GotExternalValue x -> do
        liftEff $ unsafeCoerceEff $ log $ "Got value: " <> x
        void $ T.cotransform _ { email = x }
        liftEff $ IxSignal.set (Left x) emailSignal
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (IxQueue.allowWriting updatedQueue) unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ textField
        { label
        , fullWidth
        , value: Input.valueString state.email
        , onChange: mkEffFn1 \e -> dispatch $ ChangedEmail (unsafeCoerce e).target.value
        , onBlur: mkEffFn1 \_ -> dispatch EmailUnfocused
        , error: case unsafePerformEff (IxSignal.get emailSignal) of
          Left _ -> false
          Right mEmail -> case mEmail of
            Nothing -> true
            Just e -> case parentSignal of
              Nothing -> false
              Just parentSignal' -> case unsafePerformEff (IxSignal.get parentSignal') of
                Left _ -> true
                Right mEmailParent -> case mEmailParent of
                  Nothing -> true
                  Just e2 -> e /= e2
        , name
        , id
        } []
      ]



email :: forall eff
       . { label         :: R.ReactElement
         , fullWidth     :: Boolean
         , name          :: String
         , id            :: String
         , updatedQueue  :: IxQueue (read :: READ) (Effects eff) Unit
         , emailSignal   :: IxSignal (Effects eff) (Either String (Maybe EmailAddress))
         , parentSignal  :: Maybe (IxSignal (Effects eff) (Either String (Maybe EmailAddress))) --for confirm
         , setValueQueue :: Maybe (One.Queue (write :: WRITE) (Effects eff) String)
         } -> R.ReactElement
email {label,fullWidth,name,id,updatedQueue,emailSignal,parentSignal,setValueQueue} =
  let init =
        { initEmail: case unsafePerformEff (IxSignal.get emailSignal) of
            Left e -> e
            Right x -> case x of
              Nothing -> ""
              Just y -> Email.toString y
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { label
            , fullWidth
            , name
            , id
            , updatedQueue
            , emailSignal
            , parentSignal
            } ) (initialState init)
      reactSpec' =
        case setValueQueue of
          Nothing -> reactSpec
          Just setValueQueue' ->
            Queue.whileMountedOne
              (One.allowReading setValueQueue')
              (\this x -> unsafeCoerceEff $ dispatcher this $ GotExternalValue x)
              reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
