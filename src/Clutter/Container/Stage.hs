module Clutter.Container.Stage
  ( Stage
  , stageGetDefault
  , HasColor(..)
  , toStage
  , setKeyFocus
  , resetKeyFocus
  , getKeyFocus
  ) where

import Foreign.Ptr(Ptr,nullPtr)
import Foreign.ForeignPtr(ForeignPtr, newForeignPtr_)
import Foreign.Storable(peek)
import Foreign.Marshal.Utils(with)

import Clutter.Color
import Clutter.Private


--------------------------------------------------------------------------------
newtype Stage = Stage (ForeignPtr ())

instance ForeignObject Stage where rawPtr (Stage p) = p
instance Actor Stage
instance Container Stage

toStage :: SomeActor -> IO (Maybe Stage)
toStage p = do t <- withActor p getType
               ts <- clutter_stage_get_type
               return (if t == ts then Just (Stage (rawPtr p))
                                  else Nothing)

foreign import ccall "clutter_stage_get_type"
  clutter_stage_get_type :: IO GType
--------------------------------------------------------------------------------


stageGetDefault :: IO Stage
stageGetDefault = Stage `fmap` (newForeignPtr_ =<< clutter_stage_get_default)

foreign import ccall "clutter_stage_get_default"
  clutter_stage_get_default :: IO (Ptr ())


instance HasColor Stage where
  setColor s c = withPtr s $ \p -> with c (clutter_stage_set_color p)
  getColor s   = withPtr s        $ \sp ->
                with defaultColor $ \cp ->
                  do clutter_stage_get_color sp cp
                     peek cp

foreign import ccall "clutter_stage_set_color"
  clutter_stage_set_color :: Ptr () -> Ptr Color -> IO ()

foreign import ccall "clutter_stage_set_color"
  clutter_stage_get_color :: Ptr () -> Ptr Color -> IO ()



setKeyFocus :: Actor a => Stage -> a -> IO ()
setKeyFocus s a = withPtr s   $ \ps ->
                  withActor a $ \pa ->
                  clutter_stage_set_key_focus ps pa

resetKeyFocus :: Stage -> IO ()
resetKeyFocus s = withPtr s $ \ps -> clutter_stage_set_key_focus ps nullPtr

getKeyFocus :: Stage -> IO SomeActor
getKeyFocus s = withPtr s $ \ps -> someActor =<< clutter_stage_get_key_focus ps 

foreign import ccall "clutter_stage_set_key_focus"
  clutter_stage_set_key_focus :: Ptr () -> Ptr () -> IO ()

foreign import ccall "clutter_stage_get_key_focus"
  clutter_stage_get_key_focus :: Ptr () -> IO (Ptr ())


