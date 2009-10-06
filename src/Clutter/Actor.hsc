{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

module Clutter.Actor
  (
  -- ** General
    Actor
  , actorShow
  -- ** Properties
  , setSize
  , getSize
  , setPosition
  , getPosition
  , setReactive
  , getReactive

  , setName
  , getName

  , RotateAxis
  , xAxis, yAxis, zAxis
  , setRotation
  , getRotation

  -- ** Signals
  , HandlerId
  , onShow
  , onButtonPress
  , onButtonRelease
  , onPaint
  , afterPaint
  , signalDisconnect
  ) where


import Control.Monad (liftM2)
import Foreign(Storable(peek),with)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr(Ptr)

import Clutter.GLib
import Clutter.Private(ButtonEvent(..),Actor,withActor)


foreign import ccall "clutter_actor_get_name"
  clutter_actor_get_name :: Ptr () -> IO CString

getName :: Actor a => a -> IO String
getName a = withActor a $ \pa -> peekCString =<< clutter_actor_get_name pa


foreign import ccall "clutter_actor_set_name"
  clutter_actor_set_name :: Ptr () -> CString -> IO ()

setName :: Actor a => a -> String -> IO ()
setName a s = withActor a   $ \pa ->
              withCString s $ \ps -> clutter_actor_set_name pa ps




-- | Sets the actor's size request in pixels.
-- This overrides any "normal" size request the actor would have.
-- For example a text actor might normally request the size of the text;
-- this function would force a specific size instead.
--
-- If width and/or height are -1 the actor will use its "normal" size
-- request instead of overriding it, i.e. you can "unset" the size with -1.
--
-- This function sets or unsets both the minimum and natural size.
setSize       :: Actor t => t -> Float -> Float -> IO ()
setSize t x y  = withActor t $ \p -> clutter_actor_set_size p x y

foreign import ccall "clutter_actor_set_size"
  clutter_actor_set_size :: Ptr () -> Float -> Float -> IO ()

-- | Sets the actor's fixed position in pixels relative to any parent actor.
-- If a layout manager is in use, this position will override the layout
-- manager and force a fixed position.
getSize       :: Actor t => t -> IO (Float,Float)
getSize t      =
  withActor t $ \p ->
  with 0 $ \ x ->
  with 0 $ \ y -> do
    clutter_actor_get_size p x y
    liftM2 (,) (peek x) (peek y)

foreign import ccall "clutter_actor_get_size"
  clutter_actor_get_size :: Ptr () -> Ptr Float -> Ptr Float -> IO ()

setPosition   :: Actor t => t -> Float -> Float -> IO ()
setPosition t x y = withActor t $ \p -> clutter_actor_set_position p x y

foreign import ccall "clutter_actor_set_position"
  clutter_actor_set_position :: Ptr () -> Float -> Float -> IO ()

-- | This function tries to \"do what you mean\" and tell you where the actor
-- is, prior to any transformations. Retrieves the fixed position of an actor
-- in -- pixels, if one has been set; otherwise, if the allocation is valid,
-- returns the actor's allocated position; otherwise, returns 0,0. 
getPosition   :: Actor t => t -> IO (Float,Float)
getPosition t  =
  withActor t $ \p ->
  with 0 $ \ x ->
  with 0 $ \ y -> do
    clutter_actor_get_position p x y
    liftM2 (,) (peek x) (peek y)

foreign import ccall "clutter_actor_get_position"
  clutter_actor_get_position :: Ptr () -> Ptr Float -> Ptr Float -> IO ()

-- | Show an actor.
actorShow          :: Actor t => t -> IO ()
actorShow t         = withActor t clutter_actor_show

foreign import ccall "clutter_actor_show"
  clutter_actor_show :: Ptr () -> IO ()

-- | Set if the actor should be receiving events.
setReactive   :: Actor t => t -> Bool -> IO ()
setReactive t b  = withActor t $ \p -> clutter_actor_set_reactive p b

foreign import ccall "clutter_actor_set_reactive"
  clutter_actor_set_reactive :: Ptr () -> Bool -> IO ()

-- | Checks if the actor receives events.
getReactive   :: Actor t => t -> IO Bool
getReactive t = withActor t clutter_actor_get_reactive

foreign import ccall "clutter_actor_get_reactive"
  clutter_actor_get_reactive :: Ptr () -> IO Bool

newtype RotateAxis = RA CInt deriving Eq

#enum RotateAxis, RA\
  , xAxis = CLUTTER_X_AXIS\
  , yAxis = CLUTTER_Y_AXIS\
  , zAxis = CLUTTER_Z_AXIS

foreign import ccall "clutter_actor_set_rotation"
  clutter_actor_set_rotation :: Ptr () -> CInt -> Double -> Float -> Float
                             -> Float -> IO ()

setRotation :: Actor a
            => a -> RotateAxis -> Double -> Float -> Float -> Float -> IO ()
setRotation t (RA r) angle x y z = withActor t $ \p ->
  clutter_actor_set_rotation p r angle x y z

foreign import ccall "clutter_actor_get_rotation"
  clutter_actor_get_rotation :: Ptr () -> CInt -> Ptr Float -> Ptr Float
                             -> Ptr Float -> IO Double

getRotation :: Actor a
            => a -> RotateAxis -> IO (Double,Float,Float,Float)
getRotation a (RA r) =
  withActor a $ \p  ->
  with      0 $ \xp ->
  with      0 $ \yp ->
  with      0 $ \zp -> do
    d <- clutter_actor_get_rotation p r xp yp zp
    x <- peek xp
    y <- peek yp
    z <- peek zp
    return (d,x,y,z)

-- | Run the given IO action when the actor is shown.
onShow :: Actor a => a -> IO () -> IO HandlerId
onShow t x = withActor t $ \p -> signalConnect p "show" (void x)

-- | Run the given action when a mouse button is pressed on the actor.
-- The boolean returned by the handler indicates if the event was handled.
onButtonPress :: Actor a => a -> (ButtonEvent -> IO Bool) -> IO HandlerId
onButtonPress t g = withActor t $ \p -> signalConnect p "button-press-event"
                  $ ptr_bool $ \pe -> g (BE pe)

-- | Run the given action when a mouse button is released on the actor.
-- The boolean returned by the handler indicates if the event was handled.
onButtonRelease :: Actor a => a -> (ButtonEvent -> IO Bool) -> IO HandlerId
onButtonRelease t g = withActor t $ \p -> signalConnect p "button-release-event"
                    $ ptr_bool $ \pe -> g (BE pe)


-- | The paint signal is emitted each time an actor is being painted.
onPaint :: Actor a => a -> IO () -> IO HandlerId
onPaint t x = withActor t $ \p -> signalConnect p "paint" (void x)


-- | The paint signal is emitted each time an actor is being painted.
afterPaint :: Actor a => a -> IO () -> IO HandlerId
afterPaint t x = withActor t $ \p -> signalConnectAfter p "paint" (void x)

