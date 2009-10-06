{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

module Clutter.Container
  ( addActor
  , removeActor
  , findByName
  , SomeActor
  , Container
  ) where

import Clutter.Private
import Clutter.GLib(newGObject)

import Foreign.Ptr(Ptr,nullPtr)
import Foreign.C.String(CString,withCString)

foreign import ccall "clutter_container_add_actor"
  clutter_container_add_actor :: Ptr () -> Ptr () -> IO ()

addActor :: (Container c, Actor a) => c -> a -> IO ()
addActor c a = withContainer c $ \pc ->
               withActor     a $ \pa ->
               clutter_container_add_actor pc pa

foreign import ccall "clutter_container_remove_actor"
  clutter_container_remove_actor :: Ptr () -> Ptr () -> IO ()

removeActor :: (Container c, Actor a) => c -> a -> IO ()
removeActor c a = withContainer c $ \pc ->
                  withActor     a $ \pa ->
                  clutter_container_remove_actor pc pa

findByName :: (Container c) => c -> String -> IO (Maybe SomeActor)
findByName c s = withContainer c $ \pc ->
                 withCString s   $ \ps ->
                 do pa <- clutter_container_find_child_by_name pc ps
                    if pa == nullPtr
                      then return Nothing
                      else (Just . SomeActor) `fmap` newGObject pa


foreign import ccall "clutter_container_find_child_by_name"
  clutter_container_find_child_by_name :: Ptr () -> CString -> IO (Ptr ())


