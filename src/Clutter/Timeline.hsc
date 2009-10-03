{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

module Clutter.Timeline (
    Timeline
  , newTimeline

  , TimelineDirection
  , setTimelineDirection
  , getTimelineDirection
  , timelineForward
  , timelineBackward

  , startTimeline
  , pauseTimeline
  , stopTimeline
  , rewindTimeline
  ) where

import Foreign
import Foreign.C

newtype Timeline = CT (Ptr ())

foreign import ccall "clutter_timeline_new"
  clutter_timeline_new :: CUInt -> IO Timeline

-- | Create a new @Timeline@ with the specified duration.
newTimeline :: Int -> IO Timeline
newTimeline  = clutter_timeline_new . fromIntegral


newtype TimelineDirection = TD CULong

#enum TimelineDirection, TD\
  , timelineForward  = CLUTTER_TIMELINE_FORWARD\
  , timelineBackward = CLUTTER_TIMELINE_BACKWARD

-- | Set the direction of a @Timeline@.
foreign import ccall "clutter_timeline_set_direction"
  setTimelineDirection :: Timeline -> TimelineDirection -> IO ()

-- | Get the current direction of a @Timeline@.
foreign import ccall "clutter_timeline_get_direction"
  getTimelineDirection :: Timeline -> IO TimelineDirection


-- | Start a @Timeline@.
foreign import ccall "clutter_timeline_start"
  startTimeline :: Timeline -> IO ()

-- | Pause a @Timeline@ on its current frame.
foreign import ccall "clutter_timeline_pause"
  pauseTimeline :: Timeline -> IO ()

-- | Stop the @Timeline@, and move its current frame to 0.
foreign import ccall "clutter_timeline_stop"
  stopTimeline :: Timeline -> IO ()

-- | Rewind the @Timeline@ to the first frame if its direction is
-- @timelineForward@, and to the last frame if its direction is
-- @timelineBackward@.
foreign import ccall "clutter_timeline_rewind"
  rewindTimeline :: Timeline -> IO ()
