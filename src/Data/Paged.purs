module Data.Paged where

import Prelude

import Data.Enum (class Enum, succ, pred)
import Data.Foldable (class Foldable, length)
import Data.Maybe (Maybe(Just))
import Data.Natural (Natural, natToInt, intToNat)
import Data.Page (Page(Page))
import Data.SizedPage (SizedPage(SizedPage))
import Data.Tuple (Tuple(Tuple))
import Type.Proxy (Proxy)
import Type.SimpleNat (class SimpleNat, reifyNat)


-- | Adds paging views onto a foldable datatype.
-- | In its current state, it doesn't restrict the number of pages and the
-- |   page size to match `f a`. This isn't by design, but it could be useful for
-- |   paging through part of a larger data set - for example a table of 100k rows
-- |   but keeping only 100 rows in the `Paged` instance of PageSize 10.
-- |   The `Paged` instance would need to be replaced when the current page approaches
-- |   the length of `f a`.
-- | Unsure if should keep this design, or separate into `Paged` and `PartialPage`
-- | Might want to move `PageSize` to type-level, or to type-level in `Page` type.
newtype Paged count size f a = Paged (Tuple (SizedPage count size) (f a))

instance pagedShow :: (SimpleNat count, SimpleNat size, Foldable f) =>
      Show (Paged count size f a) where
  show (Paged (Tuple sp as)) =
    "(Paged " <> show (length as :: Int)
     <> " items, "
     <> show sp
     <> ")"

instance pagedEq :: (Eq a, Eq (f a)) =>
      Eq (Paged count size f a) where
  eq (Paged (Tuple sp1 as1))
     (Paged (Tuple sp2 as2)) =
     sp1 `eq` sp2 && as1 `eq` as2

-- idk what I want this to do, so I'll leave it like this.
instance pagedOrd :: (Eq a, Eq (f a), Ord (f a)) =>
      Ord (Paged count size f a) where
  compare (Paged (Tuple sp1 as1))
          (Paged (Tuple sp2 as2)) =
       sp1 `compare` sp2 <> as1 `compare` as2

instance pagedEnum :: (Ord a, Ord (f a), SimpleNat count, SimpleNat size) =>
      Enum (Paged count size f a) where
  succ :: Paged count size f a -> Maybe (Paged count size f a)
  succ (Paged (Tuple p as)) = succ p >>= \pSucc -> Just (Paged (Tuple pSucc as))
  pred :: Paged count size f a -> Maybe (Paged count size f a)
  pred (Paged (Tuple p as)) = pred p >>= \pPred -> Just (Paged (Tuple pPred as))

--instance pagedBounded :: (Bounded total, Bounded (f a), Bounded a, SimpleNat count, SimpleNat size) =>
instance pagedBounded :: (Bounded (f a), Bounded a, SimpleNat count, SimpleNat size) =>
      Bounded (Paged count size f a) where
  top :: Paged count size f a
  top = Paged (Tuple top top)
  bottom :: Paged count size f a
  bottom = Paged (Tuple bottom bottom)


-- | Make Paged for the specified collection, using its current size
-- |  to calculate the total number of pages.
mkPaged :: forall f a r.
  Foldable f =>
  f a
  -> Natural -- page size
  -> (forall count size. SimpleNat count => SimpleNat size => Paged count size f a -> r) -- existential
  -> r
mkPaged as pageSize f =
  mkPaged' as (intToNat (length as / (natToInt pageSize))) pageSize f


-- | Make Paged with specified number of pages and page size.
mkPaged' :: forall f a r.
  Foldable f =>
  f a
  -> Natural -- number of pages
  -> Natural -- page size
  -> (forall count size. SimpleNat count => SimpleNat size => Paged count size f a -> r) -- existential
  -> r
mkPaged' as pageCount pageSize f =
  reifyNat pageCount g
  where
    g :: forall count. SimpleNat count => Proxy count -> r
    g pcnProxy = reifyNat pageSize (h pcnProxy)
    h :: forall count size. SimpleNat count => SimpleNat size => Proxy count -> Proxy size -> r
    h _ _ = f $ Paged (Tuple (SizedPage (Page zero) :: (SizedPage count size)) as)
    --h _ _ = f $ Paged (Tuple ?f as)
    --x :: forall pageCountNat pageSizeNat. Proxy pageCountNat -> Proxy pageSizeNat -> r
    --x _ _ = f $ Paged (Tuple (SizedPage zero :: (SizedPage pageCountNat pageSizeNat)) as)
