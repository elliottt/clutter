{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

module Clutter.Container (
    addActor
  , removeActor
  ) where

import Clutter.Private

import Foreign.Ptr(Ptr)

foreign import ccall "clutter_container_add_actor"
  clutter_container_add_actor :: Ptr () -> Ptr () -> IO ()

addActor :: (Container c, Actor t) => c -> t -> IO ()
addActor c a = withContainer c $ \pc ->
               withActor a     $ \pa ->
               clutter_container_add_actor pc pa

foreign import ccall "clutter_container_remove_actor"
  clutter_container_remove_actor :: Ptr () -> Ptr () -> IO ()

removeActor :: (Container c, Actor a) => c -> a -> IO ()
removeActor c a = withContainer c $ \pc ->
                  withActor a     $ \pa ->
                  clutter_container_remove_actor pc pa
