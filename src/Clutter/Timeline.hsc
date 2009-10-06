{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

module Clutter.Timeline (
    Timeline
  , newTimeline
  , getProgress

  , TimelineDirection
  , setTimelineDirection
  , getTimelineDirection
  , timelineForward
  , timelineBackward

    -- * Signals
  , onCompleted
  , onMarkerReached
  , onNewFrame
  , afterNewFrame
  , onPause
  , onStart

    -- * Playback Control
  , startTimeline
  , pauseTimeline
  , stopTimeline
  , rewindTimeline
  , setLoop

    -- * Markers
  , addMarker
  , hasMarker
  , listMarkers
  , removeMarker
  , advanceToMarker
  ) where

import Clutter.GLib

import Foreign hiding (void)
import Foreign.C

newtype Timeline = T (Ptr ())

foreign import ccall "clutter_timeline_new"
  clutter_timeline_new :: CUInt -> IO Timeline

-- | Create a new 'Timeline' with the specified duration.
newTimeline :: Int -> IO Timeline
newTimeline  = clutter_timeline_new . fromIntegral


newtype TimelineDirection = TD CULong

#enum TimelineDirection, TD\
  , timelineForward  = CLUTTER_TIMELINE_FORWARD\
  , timelineBackward = CLUTTER_TIMELINE_BACKWARD

-- | Set the direction of a 'Timeline'.
foreign import ccall "clutter_timeline_set_direction"
  setTimelineDirection :: Timeline -> TimelineDirection -> IO ()

-- | Get the current direction of a 'Timeline'.
foreign import ccall "clutter_timeline_get_direction"
  getTimelineDirection :: Timeline -> IO TimelineDirection

-- | The completed event fires when the 'Timeline' reaches the number of frames
-- specified by the num-frames property.
onCompleted :: Timeline -> IO () -> IO HandlerId
onCompleted (T t) k =
  signalConnect t "completed" (void k)

-- | The marker-reached signal is emitted each time the 'Timeline' reaches a
-- marker set with 'setMarker'.  The string passed is the name of the marker, or
-- the empty string for all markers.
onMarkerReached :: Timeline -> String -> (String -> Int -> IO ())
                -> IO HandlerId
onMarkerReached (T t) e k =
  signalConnect t evt (string_int_void k)
  where
  evt | null e    = "marker-reached"
      | otherwise = "marker-reached::" ++ e

-- | The new-frame event is emitted before each frame is drawn, to give
-- animations a chance to update the scene.
onNewFrame :: Timeline -> (Int -> IO ()) -> IO HandlerId
onNewFrame (T t) k = signalConnect t "new-frame" (int_void k)

afterNewFrame :: Timeline -> (Int -> IO ()) -> IO HandlerId
afterNewFrame (T t) k = signalConnectAfter t "new-frame" (int_void k)

onPause :: Timeline -> IO () -> IO HandlerId
onPause (T t) k = signalConnect t "paused" (void k)

onStart :: Timeline -> IO () -> IO HandlerId
onStart (T t) k = signalConnect t "started" (void k)

-- | Start a 'Timeline'.
foreign import ccall "clutter_timeline_start"
  startTimeline :: Timeline -> IO ()

-- | Pause a 'Timeline' on its current frame.
foreign import ccall "clutter_timeline_pause"
  pauseTimeline :: Timeline -> IO ()

-- | Stop the 'Timeline', and move its current frame to 0.
foreign import ccall "clutter_timeline_stop"
  stopTimeline :: Timeline -> IO ()

-- | Rewind the 'Timeline' to the first frame if its direction is
-- 'timelineForward', and to the last frame if its direction is
-- 'timelineBackward'.
foreign import ccall "clutter_timeline_rewind"
  rewindTimeline :: Timeline -> IO ()

-- | Sets whether or not the 'Timeline' should loop.
foreign import ccall "clutter_timeline_set_loop"
  setLoop :: Timeline -> Bool -> IO ()

-- | The position of the 'Timeline' in a [0,1] interval.
foreign import ccall "clutter_timeline_get_progress"
  getProgress :: Timeline -> IO Double


foreign import ccall "clutter_timeline_add_marker_at_time"
  clutter_timeline_add_marker_at_time :: Timeline
                                      -> CString
                                      -> CInt
                                      -> IO ()

-- | Add a named marker to the 'Timeline' at time t.
addMarker :: Timeline -> String -> Int -> IO ()
addMarker t l i =
  withCString l (\p -> clutter_timeline_add_marker_at_time t p (fromIntegral i))

foreign import ccall "clutter_timeline_has_marker"
  clutter_timeline_has_marker :: Timeline -> CString -> IO Bool

-- | Checks whether 'Timeline' hask a marker set with the given name.
hasMarker :: Timeline -> String -> IO Bool
hasMarker t l = withCString l (clutter_timeline_has_marker t)

foreign import ccall "clutter_timeline_list_markers"
  clutter_timeline_list_markers :: Timeline -> CInt -> Ptr CInt
                                -> IO (Ptr CString)

-- | Retrieves the list of markers for a 'Timeline' at a time t.  If Nothing is
-- passed instead of a time, all the markers set for the 'Timeline' are
-- returned.
listMarkers :: Timeline -> Maybe Int -> IO [String]
listMarkers t mb = with 0 $ \len -> do
  let time = maybe (-1) fromIntegral mb
  ptrs <- clutter_timeline_list_markers t time len
  num  <- peek len
  mapM peekCString =<< peekArray (fromIntegral num) ptrs

foreign import ccall "clutter_timeline_remove_marker"
  clutter_timeline_remove_marker :: Timeline -> CString -> IO ()

-- | Removes the marker, if found, from the 'Timeline'.
removeMarker :: Timeline -> String -> IO ()
removeMarker t l = withCString l (clutter_timeline_remove_marker t)

foreign import ccall "clutter_timeline_advance_to_marker"
  clutter_timeline_advance_to_marker :: Timeline -> CString -> IO ()

-- | Advances the 'Timeline' to the given marker.
advanceToMarker :: Timeline -> String -> IO ()
advanceToMarker t l = withCString l (clutter_timeline_advance_to_marker t)
