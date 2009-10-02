module Clutter.Actor
  ( Actor(..)   -- XXX: define instances with care!
  , actorSetSize
  , actorShow
  -- signals
  , onShow
  , onButtonPress
  , signalDisconnect
  ) where


import Foreign.Ptr(Ptr)

import Clutter.Utils


class Actor t where
  fromActor :: t -> Ptr ()

actorSetSize       :: Actor t => t -> Float -> Float -> IO ()
actorSetSize t x y  = clutter_actor_set_size (fromActor t) x y

foreign import ccall "clutter_actor_set_size"
  clutter_actor_set_size :: Ptr () -> Float -> Float -> IO ()


actorShow          :: Actor t => t -> IO ()
actorShow t         = clutter_actor_show (fromActor t)

foreign import ccall "clutter_actor_show"
  clutter_actor_show :: Ptr () -> IO ()

onShow :: Actor a => a -> IO () -> IO HandlerId
onShow t x = signalConnect (fromActor t) "show" (void0 x)

onButtonPress :: Actor a => a -> (Float -> Float -> IO Bool) -> IO HandlerId
onButtonPress t g = signalConnect (fromActor t) "button-press-event"
                  $ bool1 $ simpleButtonEvent g




