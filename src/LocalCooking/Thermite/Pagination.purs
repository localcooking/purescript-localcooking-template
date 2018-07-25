module LocalCooking.Thermite.Pagination where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', (%~), matching, clonePrism, review)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy (..))
import Control.Monad.Aff (runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)

import Sparrow.Client.Queue (SparrowClientQueues, callSparrowClientQueues)
import Thermite as T
import React (ReactSpec, ReactThis, readState)
import Queue.Types (WRITE, allowReading)
import Queue.One as One



data SortOrdering = Asc | Dsc


newtype PaginationInitIn fieldLabel = PaginationInitIn (PaginationArgs fieldLabel)

newtype PaginationInitOut a = PaginationInitOut (Array a)

data PaginationDeltaIn fieldLabel
  = PaginationChangeSize Int
  | PaginationChangeIndex Int
  | PaginationResort fieldLabel SortOrdering

data PaginationDeltaOut a
  = PaginationUpdate a -- ^ Indexing is implicit by some keyed field in `a`
  | PaginationFlush (Array a)


type PaginationSparrowClientQueues fieldLabel a eff =
  SparrowClientQueues eff
    (PaginationInitIn fieldLabel) (PaginationInitOut a) (PaginationDeltaIn fieldLabel) (PaginationDeltaOut a)




type PaginationState a = Maybe (Array a)

type PaginationArgs fieldLabel =
  { field         :: fieldLabel
  , fieldOrdering :: SortOrdering
  , pageSize      :: Int
  , pageIndex     :: Int
  }

data PaginationAction a
  = Update a
  | Flush (Array a)


class Eq key <= KeyedField a key | a -> key where
  getKey :: a -> key

class WithField field fieldLabel | field -> fieldLabel where
  fieldLabel :: Proxy field -> fieldLabel


initPaginationState :: forall a. PaginationState a
initPaginationState = Nothing


performActionPaginationState :: forall key eff state props action a
                              . KeyedField a key
                             => Lens' state (PaginationState a)
                             -> Prism' action (PaginationAction a)
                             -> T.PerformAction eff state props action
performActionPaginationState getPState getPAction action props _ =
  let go :: PaginationState a -> PaginationState a
      go state = case matching (clonePrism getPAction) action of
        Left _ -> state
        Right action' -> case action' of
          Update x -> case state of
            Nothing -> state
            Just xs -> case Array.findIndex (\y -> getKey x == getKey y) xs of
              Nothing -> state -- FIXME fail?
              Just idx -> case Array.updateAt idx x xs of
                Nothing -> state
                Just xs' -> Just xs'
          Flush xs -> Just xs
  in  void (T.cotransform (getPState %~ go))


type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , console :: CONSOLE
  | eff)


whileMountedPagination :: forall a fieldLabel key eff state action props render
                        . WithField key fieldLabel
                       => { initialPageSize :: Int
                          , deltaInQueue :: One.Queue (write :: WRITE) (Effects eff) (PaginationDeltaIn fieldLabel)
                          }
                       -> PaginationSparrowClientQueues fieldLabel a (Effects eff)
                       -> Prism' action (PaginationAction a)
                       -> (ReactThis props state -> action -> Eff (Effects eff) Unit)
                       -> ReactSpec props state render (Effects eff)
                       -> ReactSpec props state render (Effects eff)
whileMountedPagination {initialPageSize,deltaInQueue} paginationQueue getPAction dispatcher reactSpec =
  reactSpec
    { componentDidMount = \this -> do
        state <- readState this
        let resolve eX = case eX of
              Left e -> throwException e
              Right mReturn -> case mReturn of
                Nothing -> pure unit -- FIXME fail?
                Just {unsubscribe,initOut: PaginationInitOut xs,deltaIn} -> do
                  dispatcher this (review (clonePrism getPAction) (Flush xs))
                  One.onQueue (allowReading deltaInQueue) deltaIn
        unsafeCoerceEff $ runAff_ resolve $
          callSparrowClientQueues paginationQueue
            ( \deltaOut -> dispatcher this $ review (clonePrism getPAction) $ case deltaOut of
                PaginationUpdate x -> Update x
                PaginationFlush xs -> Flush xs
            )
            ( PaginationInitIn
              { field: fieldLabel (Proxy :: Proxy key)
              , fieldOrdering: Dsc
              , pageSize: initialPageSize
              , pageIndex: 0
              }
            )
    , componentWillUnmount = \_ ->
        unsafeCoerceEff $ One.delQueue $ allowReading deltaInQueue
    }
