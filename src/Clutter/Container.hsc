{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

module Clutter.Container (
    addActor
  , removeActor
  ) where

import Clutter.Private

import Foreign

foreign import ccall "clutter_container_add_actor"
  clutter_container_add_actor :: Ptr () -> Ptr () -> IO ()

addActor :: (Container c, Actor t) => c -> t -> IO ()
addActor c t = clutter_container_add_actor (fromContainer c) (fromActor t)

foreign import ccall "clutter_container_remove_actor"
  clutter_container_remove_actor :: Ptr () -> Ptr () -> IO ()

removeActor :: (Container c, Actor t) => c -> t -> IO ()
removeActor c t = clutter_container_remove_actor (fromContainer c) (fromActor t)
