module Type.SimpleNat where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(Cardinality))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Natural (Natural(), intToNat, natToInt, minus)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))


-- SimpleNat
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
