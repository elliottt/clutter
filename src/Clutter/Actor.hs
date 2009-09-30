module Clutter.Actor
  ( Actor
  , setSize
  , show
  ) where

import Prelude hiding (show)
import Foreign.Ptr(Ptr)

newtype Actor = Actor (Ptr Actor)

foreign import ccall "clutter_actor_set_size"
  setSize :: Actor -> Float -> Float -> IO ()

foreign import ccall "clutter_actor_show"
  show :: Actor -> IO ()


