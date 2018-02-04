module Data.MealyMachinePager where

import Prelude

import Control.Monad.State.Class (get, put)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Cont.Trans (ContT)
import Data.Machine.Mealy (MealyT, Step(..), halt, mealy, runMealy, singleton, stepMealy)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Partial.Unsafe (unsafeCrashWith)


type Pager m i o = MealyT (PagerMonad m i o) (PagerAction m i o) (PagerOut o)

fetchPage :: forall m i o.
  Monad m => Semiring i => Eq i =>
  Paging i
  -> Fetcher m i o
  -> (PagerMonad m i o) (Tuple o (Pager m i o))
fetchPage p f = stepToTuple <$> stepMealy (SetPaging (Tuple p f)) pager

pager :: forall m i o.
  Monad m => Semiring i => Eq i =>
  Pager m i o
pager = mealy stepFn

nextPage :: forall m i o.
  Monad m => Semiring i => Eq i =>
  Pager m i o
  -> (PagerMonad m i o) (Tuple o (Pager m i o))
nextPage pager = stepToTuple <$> stepMealy Next pager

prevPage :: forall m i o.
  Monad m => Semiring i => Eq i =>
  Pager m i o
  -> (PagerMonad m i o) (Tuple o (Pager m i o))
prevPage pager = stepToTuple <$> stepMealy Prev pager

stepToTuple :: forall m i o. PagerStep m i o -> Tuple o (Pager m i o)
stepToTuple = case _ of
  Emit a pager -> Tuple a pager
  Halt -> unsafeCrashWith "Error: Pager should never halt, but it did."



data PagerAction m i o =
  SetPaging (PagerMemory m i o)
  | Next
  | Prev

-- Recommend using `Foldable f => f a` for `a`.
type PagerOut a = a
-- Recommend using `Natural` for `i`.
type Paging i =
    { page :: i
    , count :: i
    , size :: i
    , minus :: i -> i -> i -- b/c `Natural` isn't proper Ring, but can still `minus`.
    }
type PagingUpdater i = Paging i -> Paging i -- !!! Find a better way to do this, if it's every needed.
type Fetcher m i o =
  (i -> m (Tuple (PagingUpdater i) o)) -- fst is page
type PagerMemory m i o =
  Tuple
    (Paging i)
    (Fetcher m i o)

type PagerMonad m i o = StateT (PagerMemory m i o) (ContT Unit m)
type PagerStep m i o = Step (PagerMonad m i o) (PagerAction m i o) (PagerOut o)


stepFn :: forall m i o.
  Semiring i => Eq i => Monad m =>
  PagerAction m i o
  -> (PagerMonad m i o) (PagerStep m i o)
stepFn =
  let
    fetch :: PagerMemory m i o -> m (Tuple (PagingUpdater i) o)
    fetch (Tuple paging fetcher) = fetcher paging.page
    fetchUpdateMemoryAndEmit :: PagerMemory m i o -> (PagerMonad m i o) o
    fetchUpdateMemoryAndEmit memory@(Tuple paging fetcher) = do
      Tuple f a <- lift $ lift $ fetch memory
      put $ Tuple (f paging) fetcher
      pure a
  in case _ of
    SetPaging memory -> do
      a <- fetchUpdateMemoryAndEmit memory
      pure $ Emit a $ mealy stepFn

    Next -> do
      memory <- get
      let
        paging = fst memory
        nextPage :: i
        nextPage = if paging.page == paging.count
          then paging.count
          else paging.page + one
        newMemory :: PagerMemory m i o
        newMemory =
          Tuple
            paging { page = nextPage }
            (snd memory)
      a <- fetchUpdateMemoryAndEmit newMemory
      pure $ Emit a $ mealy stepFn

    Prev -> do
      memory <- get
      let
        paging = fst memory
        nextPage = if paging.page == zero
          then zero
          else paging.page `paging.minus` one
        newMemory =
          Tuple
            paging { page = nextPage }
            (snd memory)
      a <- fetchUpdateMemoryAndEmit newMemory
      pure $ Emit a $ mealy stepFn
