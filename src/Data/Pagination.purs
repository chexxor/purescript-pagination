module Data.Pagination where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(Cardinality), succ)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Natural (Natural(), intToNat, natToInt, minus)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))


newtype Page total = Page { current :: Natural, total :: Proxy total }

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
      -- Natural clamps to zero when (_`minus`1), so could avoid `Nothing` case.
      --   But maybe `Nothing` is expected to occur when bumping to edge of Enum, so keep it.
      if p.current == zero
        then Nothing
        else Just $ Page $ p { current = p.current `minus` one }

instance pageBounded :: SimpleNat total => Bounded (Page total) where
  top :: Page total
  top = Page
    { current: let total = reflectNat (Proxy :: Proxy total)
               in total `minus` one
    , total: (Proxy :: Proxy total) }
  bottom :: Page total
  bottom = Page { current: zero, total: (Proxy :: Proxy total) }

instance pageBoundedEnum :: SimpleNat total => BoundedEnum (Page total) where
  cardinality :: Cardinality (Page total)
  cardinality = Cardinality $ natToInt $ reflectNat (Proxy :: Proxy total)
  toEnum :: Int -> Maybe (Page total)
  toEnum i = Just $ Page { current: currentVal, total: (Proxy :: Proxy total) }
    where
      currentVal =
        let iNat = intToNat i
        in if iNat >= reflectNat (Proxy :: Proxy total) then iNat `minus` one else iNat
  fromEnum :: Page total -> Int
  fromEnum (Page p) = natToInt p.current

-- Try defining and using these awesome functions. (thx @rightfold)
-- pred' :: forall a. BoundedEnum a => a -> a
-- pred' = fromMaybe <*> pred
-- succ' :: forall a. BoundedEnum a => a -> a
-- succ' = fromMaybe <*> succ


-- ??? Use bodil's instead? Might be more efficient. This SimpleNat is more... simple.
-- Can define instances for bodil's typelevel Pos/Num later.
-- https://github.com/bodil/purescript-typelevel/blob/v3.0.0/src/Data/Typelevel/Num/Sets.purs

-- To do: Move this to a separate lib.

-- Nat
data Z
data S n

class SimpleNat n where
  reflectNat :: Proxy n -> Natural

instance natZ :: SimpleNat Z where
  reflectNat _ = zero
instance natInd :: SimpleNat n => SimpleNat (S n) where
  reflectNat _ = one + reflectNat (Proxy :: Proxy n)

reifyNat :: forall r. Natural -> (forall n. SimpleNat n => Proxy n -> r) -> r
reifyNat n f | n == zero = f (Proxy :: Proxy Z)
reifyNat n f = reifyNat (n `minus` one) (f <<< succProxy)
  where
    succProxy :: forall n. Proxy n -> Proxy (S n)
    succProxy Proxy = Proxy

unsafeReifyNat :: forall r. Int -> (forall n. SimpleNat n => Proxy n -> r) -> r
unsafeReifyNat n f | n < 0 = unsafeCrashWith $ show n <> " is not a Natural number."
unsafeReifyNat 0 f = f (Proxy :: Proxy Z)
unsafeReifyNat n f = unsafeReifyNat (n - 1) (f <<< succProxy)
  where
    succProxy :: forall n. Proxy n -> Proxy (S n)
    succProxy Proxy = Proxy
