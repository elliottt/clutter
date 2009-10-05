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
addActor c t = withActor t $ \p ->
                clutter_container_add_actor (fromContainer c) p

foreign import ccall "clutter_container_remove_actor"
  clutter_container_remove_actor :: Ptr () -> Ptr () -> IO ()

removeActor :: (Container c, Actor t) => c -> t -> IO ()
removeActor c t = withActor t $ \p ->
                   clutter_container_remove_actor (fromContainer c) p
