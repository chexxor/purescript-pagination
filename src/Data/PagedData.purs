module Data.PagedData where

import Prelude

import Data.Foldable (class Foldable, length)
import Data.Natural (Natural, natToInt, intToNat)
import Data.Page (Page(Page), reifyNat, class SimpleNat)
import Type.Proxy (Proxy)


newtype PageSize = PageSize Natural

-- | Manages paging info of a foldable datatype.
data PagedData total f a = PagedData (Page total) PageSize (f a)

instance pagedDataShow :: (SimpleNat total, Foldable f) =>
      Show (PagedData total f a) where
  show (PagedData page (PageSize ps) as) =
    "(PagedData " <> show (length as :: Int)
     <> " items, page size " <> show ps
     <> " " <> show (page :: Page total)
     <> " )"

-- ??? No value add from this newtype? Remove?
newtype PageCount = PageCount Natural

-- | Make PagedData for the specified collection, using its current size
-- |  to calculate the total number of pages.
mkPagedData :: forall f a r.
  Foldable f =>
  f a
  -> PageSize
  -> (forall total. PagedData total f a -> r) -- existential
  -> r
mkPagedData as ps@(PageSize pageSize) f =
  mkPagedData' as ((PageCount <<< intToNat) (length as / (natToInt pageSize))) ps f


-- | Make PagedData with specified number of pages and page size.
mkPagedData' :: forall f a r.
  Foldable f =>
  f a
  -> PageCount
  -> PageSize
  -> (forall total. PagedData total f a -> r) -- existential
  -> r
mkPagedData' as (PageCount pageCount) pageSize f =
  reifyNat pageCount f'
  where
    f' :: forall pageCountNat. Proxy pageCountNat -> r
    f' _ = f $ PagedData (Page zero :: Page pageCountNat) pageSize as
