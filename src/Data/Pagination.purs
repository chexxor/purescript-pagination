module Data.Pagination where

import Prelude

import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), succ, pred)
import Data.Maybe (Maybe(Just, Nothing))
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))


data Page total = Page { current :: Int, total :: Proxy total }

instance pageShow :: SimpleNat total => Show (Page total) where
  show (Page p) = "{ current: " <> show p.current <> ", total: " <> (show $ reflectNat p.total) <> "}"

instance pageEq :: Eq (Page total) where
  eq :: Page total -> Page total -> Boolean
  eq (Page p1) (Page p2) = p1.current `eq` p2.current
  -- Should not need to check `total` is different, because it's type-checked to be so.

instance pageOrd :: Ord (Page total) where
  compare :: Page total -> Page total -> Ordering
  compare (Page p1) (Page p2) = p1.current `compare` p2.current

instance pageEnum :: SimpleNat total => Enum (Page total) where
  succ :: Page total -> Maybe (Page total)
  succ (Page p) =
    succ p.current >>= \succCurrent ->
      if succCurrent == reflectNat (Proxy :: Proxy total)
      then Nothing
      else Just $ Page $ p { current = succCurrent }
  pred :: Page total -> Maybe (Page total)
  pred (Page p) =
    pred p.current >>= \predCurrent ->
      if predCurrent < 0
      then Nothing
      else Just $ Page $ p { current = predCurrent }

instance pageBounded :: SimpleNat total => Bounded (Page total) where
  top :: Page total
  top = Page
    { current: let current' = reflectNat (Proxy :: Proxy total)
               in if current' < 0 then 0 else current'
    , total: (Proxy :: Proxy total) }
  bottom :: Page total
  bottom = Page { current: 0, total: (Proxy :: Proxy total) }

instance pageBoundedEnum :: SimpleNat total => BoundedEnum (Page total) where
  cardinality :: Cardinality (Page total)
  cardinality = Cardinality $ reflectNat (Proxy :: Proxy total)
  toEnum :: Int -> Maybe (Page total)
  toEnum i =
    if i < 0
    then Nothing
    else reifyNat' i \natProxy -> Just
      $ Page { current: reflectNat natProxy, total: (Proxy :: Proxy total) }
  fromEnum :: Page total -> Int
  fromEnum (Page p) = p.current

-- Try defining and using these awesome functions. (thx @rightfold)
-- pred' :: forall a. BoundedEnum a => a -> a
-- pred' = fromMaybe <*> pred
-- succ' :: forall a. BoundedEnum a => a -> a
-- succ' = fromMaybe <*> succ


-- Use bodil's instead? Might be more efficient. But this SimpleNat seems more... simple.
-- https://github.com/bodil/purescript-typelevel/blob/v3.0.0/src/Data/Typelevel/Num/Sets.purs

-- Nat
data Z
data S n

class SimpleNat n where
  reflectNat :: Proxy n -> Int

instance natZ :: SimpleNat Z where
  reflectNat _ = 0
instance natInd :: SimpleNat n => SimpleNat (S n) where
  reflectNat _ = 1 + reflectNat (Proxy :: Proxy n)

-- !!! Would be nice to have a safer version of `reifyNat'`
--reifyNat :: forall r. Int -> (forall n. SimpleNat n => Maybe (Proxy n) -> r) -> r
--reifyNat n f | n < 0 = f (Nothing :: Maybe (Proxy n))
--reifyNat n f = f $ reifyNat' n \x -> Just x

reifyNat' :: forall r. Int -> (forall n. SimpleNat n => Proxy n -> r) -> r
reifyNat' n f | n < 0 = unsafeCrashWith $ show n <> " is not a Natural number."
reifyNat' 0 f = f (Proxy :: Proxy Z)
reifyNat' n f = reifyNat' (n - 1) (f <<< succProxy)
  where
    succProxy :: forall n. Proxy n -> Proxy (S n)
    succProxy Proxy = Proxy
