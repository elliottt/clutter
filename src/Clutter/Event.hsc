module Clutter.Event where

import Foreign.Ptr(Ptr,FunPtr)
import Foreign.C.Types
import Foreign.Storable(peekByteOff)
import Data.Word

import Clutter.Utils

#include <clutter/clutter.h>

newtype ButtonEvent = BE { unBE :: Ptr () }

btnHandler   :: (ButtonEvent -> IO Bool) -> IO (FunPtr ())
btnHandler f  = bool1 (\p -> f (BE p))

btnX         :: ButtonEvent -> IO Float
btnX e        = (#peek ClutterButtonEvent, x) (unBE e)

btnY         :: ButtonEvent -> IO Float
btnY e        = (#peek ClutterButtonEvent, y) (unBE e)

btnClicks    :: ButtonEvent -> IO Word
btnClicks e   = do x <- (#peek ClutterButtonEvent, click_count) (unBE e)
                   return (fromIntegral (x :: CUInt))

btnButton    :: ButtonEvent -> IO Word
btnButton e   = do x <- (#peek ClutterButtonEvent, button) (unBE e)
                   return (fromIntegral (x :: Word32))
