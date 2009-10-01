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
              => t -> EventName a -> (t -> Event a -> IO Bool)
              -> IO HandlerId
signalConnect t ev k = withCString ev $ \ str  -> do
  cbk <- mkCallback (wrapCallback k)
  HID cbk `fmap` g_signal_connect_data (castPtr (fromActor t)) str cbk
                   nullPtr nullPtr 0

wrapCallback :: Actor a => (a -> Event e -> IO Bool) -> Callback
wrapCallback f p1 p2 _ = f (toActor (castPtr p1)) (E (castPtr p2))

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
