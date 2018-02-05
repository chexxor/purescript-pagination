module Test.ExampleMealyMachinePager where

import Prelude

import Control.Monad.Cont.Trans (ContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Array (slice, length)
import Data.Either (Either(Left, Right))
import Data.Enum (class BoundedEnum, succ, pred, fromEnum, toEnum)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Natural (natToInt, intToNat, Natural, minus)
import Data.MealyMachinePager (PagingUpdater, Paging, fetchPage)
import Data.Tuple (Tuple(Tuple))
import Type.Proxy (Proxy(Proxy))
import Type.SimpleNat (S, Z, class SimpleNat, reifyNat, reflectNat)

--------
-- Example database

newtype User = User { firstName :: String, lastName :: String }

instance userShow :: Show User where
  show (User r) = "{ firstName: " <> r.firstName <> ", lastName: " <> r.lastName <> " }"

usersTable :: Array User
usersTable =
  [ (User { firstName: "John", lastName: "Doe" })
  , (User { firstName: "Alice", lastName: "Jones" })
  , (User { firstName: "Bob", lastName: "Brown" })
  , (User { firstName: "Carol", lastName: "Dunninghamsbury-Smith" })
  ]

-- Fetch a subset of a large table stored on the server which match some query arg.
newtype QueryResponse a = QueryResponse
  { meta :: { "total-rows" :: Int }
  , data :: a
  }

instance queryResponseShow :: Show a => Show (QueryResponse a) where
  show (QueryResponse j) = "{ total-rows: " <> show j.meta."total-rows"
       <> ", data: " <> show j.data <> " }"

queryResponse :: forall a. Array a -> Int -> QueryResponse (Array a)
queryResponse as total = QueryResponse
  { meta: { "total-rows": total }
  , data: as
  }


--------
-- Example app API to communicate with the database

type MyAppMonad eff = ContT Unit (Eff eff)

fetchUsers :: forall eff. Natural -> Natural -> (MyAppMonad eff) (QueryResponse (Array User))
fetchUsers offset limit =
  let startIndex = natToInt (offset * limit)
      endIndex = startIndex + (natToInt limit)
  in pure $ queryResponse (slice startIndex endIndex usersTable) (length usersTable)


--------
-- Communicate with the API through a MealyPager

userPageFetcher :: forall eff.
  Paging Natural
  -> (MyAppMonad eff) (Tuple (PagingUpdater Natural) (Array User))
userPageFetcher paging =
  queryResponseToPagerTuple <$> fetchUsers (paging.page * paging.size) paging.size
  where
  queryResponseToPagerTuple :: forall o.
    QueryResponse o
    -> Tuple (PagingUpdater Natural) o
  queryResponseToPagerTuple (QueryResponse res) =
    let --divNat nn nd = intToNat (natToInt nn  / natToInt nd)
        f :: PagingUpdater Natural
        f p = p
          -- Update the page count to reflect the now-known response total.
          --let offset = p.size * p.page
          --in p { count = intToNat $ res.meta."total-rows" `div` (natToInt p.size) }
          --in p
    in (Tuple f res.data) -- res.data


--------
-- Example using MealyPager

--main :: Eff (console :: CONSOLE) Unit
--main = do
--  firstPage <- fetchUsers 1 -- "GET /users?page[number]=1&page[size]=2 HTTP/1.1"
--  logShow $ firstPage
--  secondPage <- fetchUsers 2 -- "GET /users?page[number]=2&page[size]=2 HTTP/1.1"
--  logShow $ secondPage

main2 :: Eff (console :: CONSOLE) Unit
main2 = do
  runPager fetchPage
  Tuple o pager <- fetchPage
    { page: zero
    , count: zero
    , size: intToNat 2
    , minus: minus
    }
    userPageFetcher
  --logShow o
  pure unit
