module Clutter.Event where

import Clutter.Actor

import Foreign
import Foreign.C.String
import Foreign.C.Types

type EventName a = String

type Callback = Ptr () -> Ptr () -> Ptr () -> IO Bool

data HandlerId = HID (FunPtr Callback) CULong

newtype Event a = E (Ptr (Event a))

signalConnect :: Actor t
              => t -> EventName a -> (Event a -> IO Bool)
              -> IO HandlerId
signalConnect t ev k = withCString ev $ \ str  -> do
  cbk <- mkCallback (wrapCallback k)
  HID cbk `fmap` g_signal_connect_data (castPtr (fromActor t)) str cbk
                   nullPtr nullPtr 0

wrapCallback :: (Event e -> IO Bool) -> Callback
wrapCallback f _ p2 _ = f (E (castPtr p2))

signalDisconnect :: Actor t => t -> HandlerId -> IO ()
signalDisconnect t (HID cbk i) = do
  g_signal_handler_disconnect (castPtr (fromActor t)) i
  freeHaskellFunPtr cbk

foreign import ccall "wrapper"
  mkCallback :: Callback -> IO (FunPtr Callback)

foreign import ccall "g_signal_connect_data"
  g_signal_connect_data :: Ptr ()          -- object
                        -> CString         -- event
                        -> FunPtr Callback -- callback
                        -> Ptr ()          -- callback data
                        -> Ptr ()          -- ??
                        -> CInt            -- ??
                        -> IO CULong

foreign import ccall "g_signal_handler_disconnect"
  g_signal_handler_disconnect :: Ptr () -- object
                      -> CULong -- handler id
                      -> IO ()

{-
#enum ClutterEventType, CET\
  , CLUTTER_NOTHING\
  , CLUTTER_KEY_PRESS\
  , CLUTTER_KEY_RELEASE\
  , CLUTTER_MOTION\
  , CLUTTER_ENTER\
  , CLUTTER_LEAVE\
  , CLUTTER_BUTTON_PRESS\
  , CLUTTER_BUTTON_RELEASE\
  , CLUTTER_SCROLL\
  , CLUTTER_STAGE_STATE\
  , CLUTTER_DESTROY_NOTIFY\
  , CLUTTER_CLIENT_MESSAGE\
  , CLUTTER_DELETE
-}
