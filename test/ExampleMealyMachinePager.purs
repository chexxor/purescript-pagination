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


newtype User = User { firstName :: String, lastName :: String }

instance userShow :: Show User where
  show (User r) = "{ firstName: " <> r.firstName <> ", lastName: " <> r.lastName <> " }"

-- Fetch a subset of a large table stored on the server which match some query arg.
newtype QueryResponse a = QueryResponse
  { meta :: { "total-rows" :: Int }
  , data :: Array a
  }

instance queryResponseShow :: Show a => Show (QueryResponse a) where
  show (QueryResponse j) = "{ total-rows: " <> show j.meta."total-rows"
       <> ", data: " <> show j.data <> " }"

firstPageResponse :: QueryResponse User
firstPageResponse = QueryResponse
  { meta: { "total-rows": 100 }
  , data:
    [ (User { firstName: "John", lastName: "Doe" })
    , (User { firstName: "Alice", lastName: "Jones" })
    ]
  }

secondPageResponse :: QueryResponse User
secondPageResponse = QueryResponse
  { meta: { "total-rows": 100 }
  , data:
    [ (User { firstName: "Bob", lastName: "Brown" })
    , (User { firstName: "Carol", lastName: "Dunninghamsbury-Smith" })
    ]
  }

fetchUsers :: forall eff. Natural -> ContT Unit (Eff eff) (QueryResponse User)
fetchUsers offset = case natToInt offset of
  1 -> pure firstPageResponse
  2 -> pure secondPageResponse
  _ -> pure $ QueryResponse { meta: { "total-rows": 0}, data: [] }

fetchUserPage :: Natural -> Natural -> ContT Unit (Eff eff) (QueryResponse User)
fetchUserPage page pageSize = fetchUsers $ page * pageSize


queryResponseToPagerTuple :: forall o.
  QueryResponse o
  -> Tuple (PagingUpdater Natural) o
queryResponseToPagerTuple (QueryResponse res) =
  let divNat nn nd = intToNat (natToInt nn  / natToInt nd)
      f p =
        -- Update the page count to reflect the now-known response total.
        let offset = p.size * p.page
        in p { count = intToNat $ res.meta."total-rows" `div` (natToInt p.size) }
  in Tuple f res.data



main :: Eff (console :: CONSOLE) Unit
main = do
  firstPage <- fetchUsers 1 -- "GET /users?page[number]=1&page[size]=2 HTTP/1.1"
  logShow $ firstPage
  secondPage <- fetchUsers 2 -- "GET /users?page[number]=2&page[size]=2 HTTP/1.1"
  logShow $ secondPage


main2 :: Eff (console :: CONSOLE) Unit
main2 = do
  -- flip runContT (\res -> pure unit )
  -- $ flip evalStateT initialMemory
  -- $ stepMealy (mealy stepFn)
  Tuple o pager <- fetchPage
    { page: zero
    , count: zero
    , size: intToNat 2
    , minus: minus
    }
    queryResponseToPagerTuple
  logShow o
-- main2 = runContT do
--   firstPage <- fetchPage pagedUserFetcher
