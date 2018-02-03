module Data.SizedPage where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, succ, pred)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (class Foldable, length)
import Data.Natural (Natural, natToInt, intToNat, minus)
import Data.Page (Page(Page))
import Type.Proxy (Proxy(Proxy))
import Type.SimpleNat (class SimpleNat, reflectNat, reifyNat)


newtype SizedPage count size = SizedPage (Page count)

instance sizedPageShow :: (SimpleNat count, SimpleNat size) =>
      Show (SizedPage count size) where
  show (SizedPage p) =
    "(SizedPage "
    <> show p
    <> " page size " <> show (reflectNat (Proxy :: Proxy size))
    <> ")"

derive newtype instance sizedPageEq :: Eq (SizedPage count size)
-- instance sizedPageEq :: Eq (SizedPage count size) where
--   eq :: SizedPage count size -> SizedPage count size -> Boolean
--   eq (SizedPage p1) (SizedPage p2) = p1 `eq` p2

derive newtype instance sizedPageOrd :: Ord (SizedPage count size)
--instance sizedPageOrd :: Ord (SizedPage count size) where
--  compare :: SizedPage count size -> SizedPage count size -> Ordering
--  compare (SizedPage p1) (SizedPage p2) = p1 `compare` p2

-- Need type-level Natural to do Enum

derive newtype instance sizedPageEnum :: (SimpleNat count, SimpleNat size) =>
    Enum (SizedPage count size)
--instance sizedPageEnum :: (SimpleNat count, SimpleNat size) => Enum (SizedPage count size) where
--  succ :: SizedPage count size -> Maybe (SizedPage count size)
--  succ (Page p) =
--    if p >= reflectNat (Proxy :: Proxy total)
--      then Nothing
--      else Just $ SizedPage (p + one)
--  pred :: SizedPage count size -> Maybe (SizedPage count size)
--  pred (Page p) =
--      if p == zero
--        then Nothing
--        else Just $ SizedPage $ p `minus` one

derive newtype instance sizedPageBounded :: (SimpleNat count, SimpleNat size) =>
    Bounded (SizedPage count size)

derive newtype instance pageBoundedEnum :: (SimpleNat count, SimpleNat size) =>
    BoundedEnum (SizedPage count size)
