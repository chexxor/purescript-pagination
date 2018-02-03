module Test.Main where

import Prelude

import Data.Array (slice)
import Data.Enum (class BoundedEnum, succ, fromEnum, toEnum)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Natural (natToInt, intToNat)
import Data.Page (Page(Page))
import Data.PagedData (PagedData(..), PageSize(..))
import Data.Paged (Paged, mkPaged)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Type.SimpleNat (S, Z, class SimpleNat)


-- Make a page2Of3, starting at page 2, of total size 3.
-- (Count starts at zero, as natural numbers do.)
type Nat0 = Z
type Nat1 = (S Z)
type Nat2 = (S (S Z))
type Nat3 = (S (S (S Z)))
type Nat4 = (S (S (S (S Z))))
type Nat5 = (S (S (S (S (S Z)))))
type Nat6 = (S (S (S (S (S (S Z))))))
page2Of3 :: Maybe (Page Nat3)
page2Of3 = toEnum 1

-- Can go to the next page. Simply asking for the next page
-- might fail, as it could be going past the last page, so `Maybe`.
page2Of3Succ :: Maybe (Page Nat3)
page2Of3Succ = page2Of3 >>= succ
page2Of3SuccSucc :: Maybe (Page Nat3)
page2Of3SuccSucc = page2Of3 >>= succ >>= succ

-- Can get the current page number from the page2Of3.
page2Of3SuccPage :: Maybe Int
page2Of3SuccPage = page2Of3 >>= succ <#> fromEnum

-- Because Page is a BoundedEnum, we can use the great helper function
succ' :: forall a. BoundedEnum a => a -> a
succ' = fromMaybe <*> succ

--   which never goes past the end, and doesn't create a `Maybe`.
page2Of3Succ' :: Maybe (Page Nat3)
page2Of3Succ' = succ' page2Of3
page2Of3Succ'Succ' :: Maybe (Page Nat3)
page2Of3Succ'Succ' = succ' $ succ' page2Of3


-- Practical example

newtype User = User { firstName :: String, lastName :: String }
instance userShow :: Show User where
  show (User r) = "{ firstName: " <> r.firstName <> ", lastName: " <> r.lastName <> " }"

users :: Array User
users =
  [ (User { firstName: "John", lastName: "Doe" })
  , (User { firstName: "Alice", lastName: "Doe" })
  , (User { firstName: "Bob", lastName: "Doe" })
  , (User { firstName: "Carol", lastName: "Doe" })
  ]

pagedUsers :: PagedData Nat2 Array User
pagedUsers = PagedData (Page (intToNat 0)) (PageSize (intToNat 3)) users

pagedUsers' :: String
--pagedUsers' :: Paged Nat2 Nat2 Array User
-- pagedUsers' :: forall count size. Paged count size Array User
pagedUsers' = mkPaged users (intToNat 2) f
  where
    f :: forall count size. SimpleNat count => SimpleNat size =>
        Paged count size Array User
        -> String
        -- -> Paged count size Array User
    f paged = show paged

getCurrentPage :: forall total a. PagedData total Array a -> Array a
getCurrentPage (PagedData (Page p) (PageSize ps) as) =
  let offset = natToInt $ p * ps
  in slice offset (natToInt ps) as

nextPage :: forall total a. SimpleNat total => PagedData total Array a -> PagedData total Array a
nextPage (PagedData p pSize as) = maybe (f p) f (succ p)
  where f newP = PagedData newP pSize as


main :: Eff (console :: CONSOLE) Unit
main = do
  logShow page2Of3
  -- (Just (Page 2 of 3))
  logShow page2Of3Succ
  -- (Just (Page 3 of 3)
  logShow page2Of3SuccSucc
  -- Nothing
  logShow page2Of3SuccPage
  -- (Just 2)
  logShow page2Of3Succ'
  -- (Just (Page 3 of 3))
  logShow page2Of3Succ'Succ'
  -- (Just (Page 3 of 3))
  logShow $ ((toEnum $ fromEnum $ page2Of3) :: Maybe (Page Nat3))
  -- (Just (Page 3 of 3))
  logShow $ pagedUsers
 -- (PagedData 4 items, page size 3 (Page 1 of 2) )
  logShow $ pagedUsers'
  -- "(Paged 4 items, (SizedPage 2 pages, page size 2) )"
