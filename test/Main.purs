module Test.Main where

import Prelude

import Data.Enum (class BoundedEnum, succ, fromEnum, toEnum)
import Data.Maybe (Maybe, fromMaybe)
import Data.Pagination (Page, S, Z)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)



-- Make a pager, starting at page 2, of total size 3.
-- (Count starts at zero, as natural numbers do.)
type PageCount = (S (S (S Z)))
pager :: Maybe (Page PageCount)
pager = toEnum 1

-- Can go to the next page. Simply asking for the next page
-- might fail, as it could be going past the last page, so `Maybe`.
pagerSucc :: Maybe (Page PageCount)
pagerSucc = pager >>= succ
pagerSuccSucc :: Maybe (Page PageCount)
pagerSuccSucc = pager >>= succ >>= succ

-- Can get the current page number from the pager.
pagerSuccPage :: Maybe Int
pagerSuccPage = pager >>= succ <#> fromEnum

-- Because Page is a BoundedEnum, we can use the great helper function
succ' :: forall a. BoundedEnum a => a -> a
succ' = fromMaybe <*> succ

--   which never goes past the end, and doesn't create a `Maybe`.
pagerSucc' :: Maybe (Page PageCount)
pagerSucc' = succ' pager
pagerSucc'Succ' :: Maybe (Page PageCount)
pagerSucc'Succ' = succ' $ succ' pager


main :: Eff (console :: CONSOLE) Unit
main = do
  logShow pager
  -- (Just { current: 1, total: 3})
  logShow pagerSucc
  -- (Just { current: 2, total: 3})
  logShow pagerSuccSucc
  -- Nothing
  logShow pagerSuccPage
  -- (Just 2)
  logShow pagerSucc'
  -- (Just { current: 2, total: 3})
  logShow pagerSucc'Succ'
  -- (Just { current: 2, total: 3})
