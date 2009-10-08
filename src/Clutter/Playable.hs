module Clutter.Playable where

class Playable a where
  start     :: a -> IO ()
  pause     :: a -> IO ()
  stop      :: a -> IO ()
  isPlaying :: a -> IO Bool
  rewind    :: a -> IO ()
  setLoop   :: a -> Bool -> IO ()
  getLoop   :: a -> IO Bool
