module Clutter.Actor
  ( IsActor(..)   -- XXX: define instances with care!
  , actorSetSize
  , actorShow
  ) where

import Prelude hiding (show)
import Foreign.Ptr(Ptr)


class IsActor t where
  toPtr :: t -> Ptr ()

actorSetSize       :: IsActor t => t -> Float -> Float -> IO ()
actorSetSize t x y  = clutter_actor_set_size (toPtr t) x y

foreign import ccall "clutter_actor_set_size"
  clutter_actor_set_size :: Ptr () -> Float -> Float -> IO ()


actorShow          :: IsActor t => t -> IO ()
actorShow t         = clutter_actor_show (toPtr t)

foreign import ccall "clutter_actor_show"
  clutter_actor_show :: Ptr () -> IO ()





