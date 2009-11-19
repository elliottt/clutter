{-# LANGUAGE ForeignFunctionInterface #-}

module Clutter.Timeline (
    -- * Timelines
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

    -- * Markers
  , addMarker
  , hasMarker
  , listMarkers
  , removeMarker
  , advanceToMarker

    -- * Scores
  , Score
  , ScoreId
  , newScore
  , startWith
  , sequenceTimelines
  , removeTimeline
  , removeAllTimelines
  ) where

import Clutter.Playable
import Clutter.Private

import Foreign hiding (void)
import Foreign.C


-- Timelines -------------------------------------------------------------------

foreign import ccall "clutter_timeline_new"
  clutter_timeline_new :: CUInt -> IO (Ptr ())

-- | Create a new 'Timeline' with the specified duration.
newTimeline :: Int -> IO Timeline
newTimeline t = ptrTimeline =<< clutter_timeline_new (fromIntegral t)

-- | Set the direction of a 'Timeline'.
setTimelineDirection :: Timeline -> TimelineDirection -> IO ()
setTimelineDirection t d =
  withPtr t (\p -> clutter_timeline_set_direction p d)

foreign import ccall "clutter_timeline_set_direction"
  clutter_timeline_set_direction :: Ptr () -> TimelineDirection -> IO ()


-- | Get the current direction of a 'Timeline'.
getTimelineDirection :: Timeline -> IO TimelineDirection
getTimelineDirection t = withPtr t clutter_timeline_get_direction

foreign import ccall "clutter_timeline_get_direction"
  clutter_timeline_get_direction :: Ptr () -> IO TimelineDirection


-- | The completed event fires when the 'Timeline' reaches the number of frames
-- specified by the num-frames property.
onCompleted :: Timeline -> IO () -> IO HandlerId
onCompleted t k = withPtr t (\p -> signalConnect p "completed" (void k))

-- | The marker-reached signal is emitted each time the 'Timeline' reaches a
-- marker set with 'setMarker'.  The string passed is the name of the marker, or
-- the empty string for all markers.
onMarkerReached :: Timeline -> String -> (String -> Int -> IO ())
                -> IO HandlerId
onMarkerReached t e k =
  withPtr t (\p -> signalConnect p evt (string_int_void k))
  where
  evt | null e    = "marker-reached"
      | otherwise = "marker-reached::" ++ e

-- | The new-frame event is emitted before each frame is drawn, to give
-- animations a chance to update the scene.
onNewFrame :: Timeline -> (Int -> IO ()) -> IO HandlerId
onNewFrame t k = withPtr t (\p -> signalConnect p "new-frame" (int_void k))

afterNewFrame :: Timeline -> (Int -> IO ()) -> IO HandlerId
afterNewFrame t k =
  withPtr t (\p -> signalConnectAfter p "new-frame" (int_void k))

onPause :: Timeline -> IO () -> IO HandlerId
onPause t k = withPtr t (\p -> signalConnect p "paused" (void k))

onStart :: Timeline -> IO () -> IO HandlerId
onStart t k = withPtr t (\p -> signalConnect p "started" (void k))

instance Playable Timeline where
  start     t   = withPtr t clutter_timeline_start
  pause     t   = withPtr t clutter_timeline_pause
  stop      t   = withPtr t clutter_timeline_stop
  rewind    t   = withPtr t clutter_timeline_rewind
  isPlaying t   = withPtr t clutter_timeline_is_playing
  setLoop   t b = withPtr t (\p -> clutter_timeline_set_loop p b)
  getLoop   t   = withPtr t clutter_timeline_get_loop

foreign import ccall "clutter_timeline_start"
  clutter_timeline_start :: Ptr () -> IO ()

foreign import ccall "clutter_timeline_pause"
  clutter_timeline_pause :: Ptr () -> IO ()

foreign import ccall "clutter_timeline_stop"
  clutter_timeline_stop :: Ptr () -> IO ()

foreign import ccall "clutter_timeline_rewind"
  clutter_timeline_rewind :: Ptr () -> IO ()

foreign import ccall "clutter_timeline_is_playing"
  clutter_timeline_is_playing :: Ptr () -> IO Bool

foreign import ccall "clutter_timeline_set_loop"
  clutter_timeline_set_loop :: Ptr () -> Bool -> IO ()

foreign import ccall "clutter_timeline_get_loop"
  clutter_timeline_get_loop :: Ptr () -> IO Bool

-- | The position of the 'Timeline' in a [0,1] interval.
getProgress :: Timeline -> IO Double
getProgress t = withPtr t clutter_timeline_get_progress

foreign import ccall "clutter_timeline_get_progress"
  clutter_timeline_get_progress :: Ptr () -> IO Double


foreign import ccall "clutter_timeline_add_marker_at_time"
  clutter_timeline_add_marker_at_time :: Ptr ()
                                      -> CString
                                      -> CInt
                                      -> IO ()

-- | Add a named marker to the 'Timeline' at time t.
addMarker :: Timeline -> String -> Int -> IO ()
addMarker t l i =
  withCString l $ \ str ->
  withPtr     t $ \ p   ->
    clutter_timeline_add_marker_at_time p str (fromIntegral i)

foreign import ccall "clutter_timeline_has_marker"
  clutter_timeline_has_marker :: Ptr () -> CString -> IO Bool

-- | Checks whether 'Timeline' hask a marker set with the given name.
hasMarker :: Timeline -> String -> IO Bool
hasMarker t l =
  withCString l $ \ str ->
  withPtr     t $ \ p   ->
    clutter_timeline_has_marker p str

foreign import ccall "clutter_timeline_list_markers"
  clutter_timeline_list_markers :: Ptr () -> CInt -> Ptr CInt
                                -> IO (Ptr CString)

-- | Retrieves the list of markers for a 'Timeline' at a time t.  If Nothing is
-- passed instead of a time, all the markers set for the 'Timeline' are
-- returned.
listMarkers :: Timeline -> Maybe Int -> IO [String]
listMarkers t mb =
  with    0 $ \ len ->
  withPtr t $ \ p   -> do
    let time = maybe (-1) fromIntegral mb
    ptrs <- clutter_timeline_list_markers p time len
    num  <- peek len
    mapM peekCString =<< peekArray (fromIntegral num) ptrs

foreign import ccall "clutter_timeline_remove_marker"
  clutter_timeline_remove_marker :: Ptr () -> CString -> IO ()

-- | Removes the marker, if found, from the 'Timeline'.
removeMarker :: Timeline -> String -> IO ()
removeMarker t l =
  withCString l $ \ str ->
  withPtr     t $ \ p   ->
    clutter_timeline_remove_marker p str

foreign import ccall "clutter_timeline_advance_to_marker"
  clutter_timeline_advance_to_marker :: Ptr () -> CString -> IO ()

-- | Advances the 'Timeline' to the given marker.
advanceToMarker :: Timeline -> String -> IO ()
advanceToMarker t l =
  withCString l $ \ str ->
  withPtr     t $ \ p   ->
    clutter_timeline_advance_to_marker p str


-- Scores ----------------------------------------------------------------------

newtype Score = S (ForeignPtr ())

instance ForeignObject Score where
  rawPtr (S p) = p

foreign import ccall "clutter_score_new"
  clutter_score_new :: IO (Ptr ())

-- | Create a new score.
newScore :: IO Score
newScore  = S `fmap` (newGObject =<< clutter_score_new)

instance Playable Score where
  start     s   = withPtr s clutter_score_start
  pause     s   = withPtr s clutter_score_pause
  stop      s   = withPtr s clutter_score_stop
  rewind    s   = withPtr s clutter_score_rewind
  isPlaying s   = withPtr s clutter_score_is_playing
  setLoop   s b = withPtr s (\p -> clutter_score_set_loop p b)
  getLoop   s   = withPtr s clutter_score_get_loop

foreign import ccall "clutter_score_start"
  clutter_score_start :: Ptr () -> IO ()

foreign import ccall "clutter_score_pause"
  clutter_score_pause :: Ptr () -> IO ()

foreign import ccall "clutter_score_stop"
  clutter_score_stop :: Ptr () -> IO ()

foreign import ccall "clutter_score_rewind"
  clutter_score_rewind :: Ptr () -> IO ()

foreign import ccall "clutter_score_is_playing"
  clutter_score_is_playing :: Ptr () -> IO Bool

foreign import ccall "clutter_score_set_loop"
  clutter_score_set_loop :: Ptr () -> Bool -> IO ()

foreign import ccall "clutter_score_get_loop"
  clutter_score_get_loop :: Ptr () -> IO Bool

foreign import ccall "clutter_score_append"
  clutter_score_append :: Ptr () -> Ptr () -> Ptr () -> IO ScoreId

newtype ScoreId = SI CULong

-- | Start a score by playing the given timeline.
startWith :: Score -> Timeline -> IO ScoreId
startWith s t =
  withPtr s $ \ps ->
  withPtr t $ \pt ->
    clutter_score_append ps nullPtr pt

-- | Sequence two timelines in a score.
sequenceTimelines :: Score -> Timeline -> Timeline -> IO ScoreId
sequenceTimelines s ta tb =
  withPtr s  $ \ps  ->
  withPtr ta $ \pta ->
  withPtr tb $ \ptb ->
    clutter_score_append ps pta ptb

-- | Remove a timeline, by score id, from the score.
removeTimeline :: Score -> ScoreId -> IO ()
removeTimeline s i = withPtr s (\p -> clutter_score_remove p i)

foreign import ccall "clutter_score_remove"
  clutter_score_remove :: Ptr () -> ScoreId -> IO ()

-- | Clear a score of all timelines.
removeAllTimelines :: Score -> IO ()
removeAllTimelines s = withPtr s clutter_score_remove_all

foreign import ccall "clutter_score_remove_all"
  clutter_score_remove_all :: Ptr () -> IO ()
