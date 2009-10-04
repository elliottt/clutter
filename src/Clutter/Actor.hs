module Clutter.Actor
  ( Actor
  , actorSetSize
  , actorGetSize
  , actorSetPosition
  , actorGetPosition
  , actorShow
  , actorSetReactive
  , actorGetReactive
  -- signals
  , onShow
  , onButtonPress
  , onButtonRelease
  , signalDisconnect
  ) where


import Control.Monad (liftM2)
import Foreign(Storable(peek),with)
import Foreign.Ptr(Ptr)

import Clutter.Utils
import Clutter.Private


actorSetSize       :: Actor t => t -> Float -> Float -> IO ()
actorSetSize t x y  = clutter_actor_set_size (fromActor t) x y

foreign import ccall "clutter_actor_set_size"
  clutter_actor_set_size :: Ptr () -> Float -> Float -> IO ()

actorGetSize       :: Actor t => t -> IO (Float,Float)
actorGetSize t      =
  with 0 $ \ x ->
  with 0 $ \ y -> do
    clutter_actor_get_size (fromActor t) x y
    liftM2 (,) (peek x) (peek y)

foreign import ccall "clutter_actor_get_size"
  clutter_actor_get_size :: Ptr () -> Ptr Float -> Ptr Float -> IO ()

actorSetPosition   :: Actor t => t -> Float -> Float -> IO ()
actorSetPosition t x y = clutter_actor_set_position (fromActor t) x y

foreign import ccall "clutter_actor_set_position"
  clutter_actor_set_position :: Ptr () -> Float -> Float -> IO ()

actorGetPosition   :: Actor t => t -> IO (Float,Float)
actorGetPosition t  =
  with 0 $ \ x ->
  with 0 $ \ y -> do
    clutter_actor_get_position (fromActor t) x y
    liftM2 (,) (peek x) (peek y)

foreign import ccall "clutter_actor_get_position"
  clutter_actor_get_position :: Ptr () -> Ptr Float -> Ptr Float -> IO ()

actorShow          :: Actor t => t -> IO ()
actorShow t         = clutter_actor_show (fromActor t)

foreign import ccall "clutter_actor_show"
  clutter_actor_show :: Ptr () -> IO ()

actorSetReactive   :: Actor t => t -> Bool -> IO ()
actorSetReactive t  = clutter_actor_set_reactive (fromActor t)

foreign import ccall "clutter_actor_set_reactive"
  clutter_actor_set_reactive :: Ptr () -> Bool -> IO ()

actorGetReactive   :: Actor t => t -> IO Bool
actorGetReactive t = clutter_actor_get_reactive (fromActor t)

foreign import ccall "clutter_actor_get_reactive"
  clutter_actor_get_reactive :: Ptr () -> IO Bool

onShow :: Actor a => a -> IO () -> IO HandlerId
onShow t x = signalConnect (fromActor t) "show" (void0 x)

onButtonPress :: Actor a => a -> (ButtonEvent -> IO Bool) -> IO HandlerId
onButtonPress t g = signalConnect (fromActor t) "button-press-event"
                  $ bool1 $ \p -> g (BE p)

onButtonRelease :: Actor a => a -> (ButtonEvent -> IO Bool) -> IO HandlerId
onButtonRelease t g = signalConnect (fromActor t) "button-release-event"
                    $ bool1 $ \p -> g (BE p)





