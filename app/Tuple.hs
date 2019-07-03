{-# LANGUAGE DeriveFunctor #-}
module Tuple where

import Control.Monad.Trans.Free
import Control.Monad.Trans.State.Strict hiding (get, put)
import           Control.Monad             (when)
import           Control.Monad.Trans.Class (lift)

-- DSL for a tuple of <String, Int>

type Tuple1 = (String,Int)

data TupleF k =
    Get (Tuple1 -> k)
  | Query (Tuple1 -> k)
  | Put Tuple1 (Bool -> k)
  deriving Functor

type TupleT m a = FreeT TupleF m a

get :: Monad m => TupleT m Tuple1
get = liftF $ Get id

query :: Monad m => TupleT m Tuple1
query = liftF $ Query id

put :: Monad m => Tuple1 -> TupleT m Bool
put x = liftF $ Put x id

counter :: Monad m => TupleT m Int
counter = do
  put ("sum", 0)
  (_,v) <- get
  let v' = v + 1
  put ("sum", v')
  (_,v'') <- get
  return v''

interpret :: Monad m => [Tuple1] -> TupleT m r -> m r
interpret tupleSpace a = do
  mr <- runFreeT a
  case mr of
    Pure r -> return r
    Free (Get k) ->
      interpret [] (k (head tupleSpace))
    Free (Query k) ->
      let tuple = head tupleSpace
       in interpret tupleSpace (k tuple)
    Free (Put x k) ->
      let
        test = if (tupleSpace == [])
               then True
               else x /= (head tupleSpace)
        next  = if test then (x:tupleSpace) else tupleSpace
       in interpret next (k test)

test :: IO ()
test = do
  r <- interpret [] counter
  print r
