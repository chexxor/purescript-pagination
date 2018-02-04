module Data.Page where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(Cardinality))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Natural (Natural(), intToNat, natToInt, minus)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Type.SimpleNat (class SimpleNat, reflectNat, reifyNat)


newtype Page total = Page Natural

instance pageShow :: SimpleNat total => Show (Page total) where
  show (Page p) =
    "(Page " <> show (p + one)
    <> " of " <> (show $ (_ + one) $ reflectNat (Proxy :: Proxy total)) <> ")"

instance pageEq :: Eq (Page total) where
  eq :: Page total -> Page total -> Boolean
  eq (Page p1) (Page p2) = p1 `eq` p2
  -- Should not need to check `total` is different, because it's type-checked to be so.

instance pageOrd :: Ord (Page total) where
  compare :: Page total -> Page total -> Ordering
  compare (Page p1) (Page p2) = p1 `compare` p2


-- Enum, Bounded, and BoundedEnum instances require a type-level Natural number implementation

-- ??? Use bodil's instead? Might be more efficient. Only using SimpleNat b/c is more... simple.
-- Can define instances for bodil's typelevel Pos/Num later.
-- https://github.com/bodil/purescript-typelevel/blob/v3.0.0/src/Data/Typelevel/Num/Sets.purs

instance pageEnum :: SimpleNat total => Enum (Page total) where
  succ :: Page total -> Maybe (Page total)
  succ (Page p) =
    if p >= reflectNat (Proxy :: Proxy total)
      then Nothing
      else Just $ Page (p + one)
  pred :: Page total -> Maybe (Page total)
  pred (Page p) =
      -- Natural clamps to zero when (_`minus`1), so could avoid `Nothing` case.
      --   But maybe `Nothing` is expected to occur when bumping to edge of Enum, so keep it.
      if p == zero
        then Nothing
        else Just $ Page $ p `minus` one

instance pageBounded :: SimpleNat total => Bounded (Page total) where
  top :: Page total
  top = Page $ reflectNat (Proxy :: Proxy total) `minus` one
  bottom :: Page total
  bottom = Page zero

instance pageBoundedEnum :: SimpleNat total => BoundedEnum (Page total) where
  cardinality :: Cardinality (Page total)
  cardinality = Cardinality $ natToInt $ reflectNat (Proxy :: Proxy total)
  toEnum :: Int -> Maybe (Page total)
  toEnum i = Just $ Page p
    where
      p = let iNat = intToNat i
          in if iNat >= reflectNat (Proxy :: Proxy total) then iNat `minus` one else iNat
  fromEnum :: Page total -> Int
  fromEnum (Page p) = natToInt p

-- Try defining and using these awesome functions. (thx @rightfold)
-- pred' :: forall a. BoundedEnum a => a -> a
-- pred' = fromMaybe <*> pred
-- succ' :: forall a. BoundedEnum a => a -> a
-- succ' = fromMaybe <*> succ


