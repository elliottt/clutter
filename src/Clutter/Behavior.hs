module Clutter.Behavior
  ( Behavior
  , SomeBehavior
  , apply
  , isApplied
  , remove
  , removeAll
  , getActorNum
  , getActor
  , foreachActor
  , Alpha
  , getAlpha
  , setAlpha
  , removeAlpha
  ) where

import Clutter.Private

import Data.Word
import Foreign.Ptr
import Foreign.C.Types

apply          :: (Behavior b, Actor a) => b -> a -> IO ()
apply b a       = withBehavior b $ \pb ->
                  withActor    a $ \pa -> clutter_behaviour_apply pb pa

isApplied      :: (Behavior b, Actor a) => b -> a -> IO Bool
isApplied b a   = withBehavior b $ \pb ->
                  withActor    a $ \pa -> clutter_behaviour_is_applied pb pa

remove         :: (Behavior b, Actor a) => b -> a -> IO ()
remove b a      = withBehavior b $ \pb ->
                  withActor    a $ \pa -> clutter_behaviour_remove pb pa

removeAll      :: (Behavior b) => b -> IO ()
removeAll b     = withBehavior b $ \pb -> clutter_behaviour_remove_all pb

getActorNum    :: (Behavior b) => b -> IO Word
getActorNum b   = withBehavior b $ \pb ->
                    fromIntegral `fmap` clutter_behaviour_get_n_actors pb

getActor       :: (Behavior b) => b -> Word -> IO (Maybe SomeActor)
getActor b n    = withBehavior b $ \pb ->
                    do a <- clutter_behaviour_get_nth_actor pb (fromIntegral n)
                       if a == nullPtr then return Nothing 
                                       else Just `fmap` someActor a

foreachActor    :: (Behavior b) => b -> (SomeActor -> IO ()) -> IO ()
foreachActor b f = withBehavior b $ \pb ->
                    do ff <- new_foreach_fun (\p -> f =<< someActor p)
                       clutter_behaviour_actors_foreach pb ff nullPtr
                       freeHaskellFunPtr ff

getAlpha       :: (Behavior b) => b -> IO (Maybe Alpha)
getAlpha b      = withBehavior b $ \pb ->
                    do a <- clutter_behaviour_get_alpha pb
                       if a == nullPtr then return Nothing
                                       else Just `fmap` ptrAlpha a

setAlpha       :: (Behavior b) => b -> Alpha -> IO ()
setAlpha b a    = withBehavior b  $ \pb ->
                  withAlpha a     $ \pa ->
                    clutter_behaviour_set_alpha pb pa

removeAlpha    :: (Behavior b) => b -> IO ()
removeAlpha b   = withBehavior b $ \pb ->
                    clutter_behaviour_set_alpha pb nullPtr

--------------------------------------------------------------------------------

foreign import ccall "clutter_behaviour_apply"
  clutter_behaviour_apply :: Ptr () -> Ptr () -> IO ()

foreign import ccall "clutter_behaviour_remove"
  clutter_behaviour_remove :: Ptr () -> Ptr () -> IO ()

foreign import ccall "clutter_behaviour_remove_all"
  clutter_behaviour_remove_all :: Ptr () -> IO ()

foreign import ccall "clutter_behaviour_is_applied"
  clutter_behaviour_is_applied :: Ptr () -> Ptr () -> IO Bool

foreign import ccall "wrapper"
  new_foreach_fun :: (Ptr () -> IO ()) -> IO (FunPtr (Ptr () -> IO ()))

foreign import ccall "clutter_behaviour_actors_foreach"
  clutter_behaviour_actors_foreach :: Ptr () -> FunPtr (Ptr () -> IO ())
                                   -> Ptr () -> IO ()

foreign import ccall "clutter_behaviour_get_n_actors"
  clutter_behaviour_get_n_actors :: Ptr () -> IO CInt

foreign import ccall "clutter_behaviour_get_nth_actor"
  clutter_behaviour_get_nth_actor :: Ptr () -> Int -> IO (Ptr ())

foreign import ccall "clutter_behaviour_get_alpha"
  clutter_behaviour_get_alpha :: Ptr () -> IO (Ptr ())

foreign import ccall "clutter_behaviour_set_alpha"
  clutter_behaviour_set_alpha :: Ptr () -> Ptr () -> IO ()

