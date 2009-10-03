module Clutter.Actor
  ( Actor
  , actorSetSize
  , actorShow
  -- signals
  , onShow
  , onButtonPress
  , onButtonRelease
  , signalDisconnect
  ) where


import Foreign.Ptr(Ptr)

import Clutter.Utils
import Clutter.Private


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

onButtonPress :: Actor a => a -> (ButtonEvent -> IO Bool) -> IO HandlerId
onButtonPress t g = signalConnect (fromActor t) "button-press-event"
                  $ bool1 $ \p -> g (BE p)

onButtonRelease :: Actor a => a -> (ButtonEvent -> IO Bool) -> IO HandlerId
onButtonRelease t g = signalConnect (fromActor t) "button-release-event"
                    $ bool1 $ \p -> g (BE p)





