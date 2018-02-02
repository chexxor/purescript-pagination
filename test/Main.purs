module Test.Main where

import Prelude

import Data.Enum (class BoundedEnum, succ, fromEnum, toEnum)
import Data.Maybe (Maybe, fromMaybe)
import Data.Pagination (Page, S, Z)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)



-- Make a page2Of3, starting at page 2, of total size 3.
-- (Count starts at zero, as natural numbers do.)
type Nat3 = (S (S (S Z)))
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
