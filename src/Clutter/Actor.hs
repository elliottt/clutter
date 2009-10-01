module Clutter.Actor
  ( Actor(..)   -- XXX: define instances with care!
  , actorSetSize
  , actorShow
  ) where

import Prelude hiding (show)
import Foreign.Ptr(Ptr)


class Actor t where
  fromActor :: t -> Ptr t
  toActor   :: Ptr t -> t

actorSetSize       :: Actor t => t -> Float -> Float -> IO ()
actorSetSize t x y  = clutter_actor_set_size (fromActor t) x y

foreign import ccall "clutter_actor_set_size"
  clutter_actor_set_size :: Ptr t -> Float -> Float -> IO ()


actorShow          :: Actor t => t -> IO ()
actorShow t         = clutter_actor_show (fromActor t)

foreign import ccall "clutter_actor_show"
  clutter_actor_show :: Ptr t -> IO ()





