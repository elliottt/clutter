module Clutter.Actor
  (
  -- ** General
    Actor
  , actorShow
  -- ** Properties
  , actorSetSize
  , actorGetSize
  , actorSetPosition
  , actorGetPosition
  , actorSetReactive
  , actorGetReactive
  -- ** Signals
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


-- | Sets the actor's size request in pixels.
-- This overrides any "normal" size request the actor would have.
-- For example a text actor might normally request the size of the text;
-- this function would force a specific size instead.
--
-- If width and/or height are -1 the actor will use its "normal" size
-- request instead of overriding it, i.e. you can "unset" the size with -1.
--
-- This function sets or unsets both the minimum and natural size.
actorSetSize       :: Actor t => t -> Float -> Float -> IO ()
actorSetSize t x y  = clutter_actor_set_size (fromActor t) x y

foreign import ccall "clutter_actor_set_size"
  clutter_actor_set_size :: Ptr () -> Float -> Float -> IO ()

-- | Sets the actor's fixed position in pixels relative to any parent actor.
-- If a layout manager is in use, this position will override the layout
-- manager and force a fixed position.
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

-- | This function tries to \"do what you mean\" and tell you where the actor
-- is, prior to any transformations. Retrieves the fixed position of an actor
-- in -- pixels, if one has been set; otherwise, if the allocation is valid,
-- returns the actor's allocated position; otherwise, returns 0,0. 
actorGetPosition   :: Actor t => t -> IO (Float,Float)
actorGetPosition t  =
  with 0 $ \ x ->
  with 0 $ \ y -> do
    clutter_actor_get_position (fromActor t) x y
    liftM2 (,) (peek x) (peek y)

foreign import ccall "clutter_actor_get_position"
  clutter_actor_get_position :: Ptr () -> Ptr Float -> Ptr Float -> IO ()

-- | Show an actor.
actorShow          :: Actor t => t -> IO ()
actorShow t         = clutter_actor_show (fromActor t)

foreign import ccall "clutter_actor_show"
  clutter_actor_show :: Ptr () -> IO ()

-- | Set if the actor should be receiving events.
actorSetReactive   :: Actor t => t -> Bool -> IO ()
actorSetReactive t  = clutter_actor_set_reactive (fromActor t)

foreign import ccall "clutter_actor_set_reactive"
  clutter_actor_set_reactive :: Ptr () -> Bool -> IO ()

-- | Checks if the actor receives events.
actorGetReactive   :: Actor t => t -> IO Bool
actorGetReactive t = clutter_actor_get_reactive (fromActor t)

foreign import ccall "clutter_actor_get_reactive"
  clutter_actor_get_reactive :: Ptr () -> IO Bool

-- | Run the given IO action when the actor is shown.
onShow :: Actor a => a -> IO () -> IO HandlerId
onShow t x = signalConnect (fromActor t) "show" (void0 x)

-- | Run the given action when a mouse button is pressed on the actor.
-- The boolean returned by the handler indicates if the event was handled.
onButtonPress :: Actor a => a -> (ButtonEvent -> IO Bool) -> IO HandlerId
onButtonPress t g = signalConnect (fromActor t) "button-press-event"
                  $ bool1 $ \p -> g (BE p)

-- | Run the given action when a mouse button is released on the actor.
-- The boolean returned by the handler indicates if the event was handled.
onButtonRelease :: Actor a => a -> (ButtonEvent -> IO Bool) -> IO HandlerId
onButtonRelease t g = signalConnect (fromActor t) "button-release-event"
                    $ bool1 $ \p -> g (BE p)





