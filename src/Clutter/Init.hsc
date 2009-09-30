{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Clutter.Init
  ( application

  , ClutterInitError
  , clutterInitErrorUnknown
  , clutterInitErrorThreads
  , clutterInitErrorBackend
  , clutterInitErrorInternal
  ) where

#include <clutter/clutter.h>

import Foreign.C.String
import Foreign.C.Types
import Foreign
import System.Environment(getProgName,getArgs,withArgs)
import Control.Exception(Exception,throwIO)
import Data.Typeable(Typeable)

newtype ClutterInitError = CIE CInt
  deriving (Eq, Typeable)

instance Exception ClutterInitError

instance Show ClutterInitError where
  show e
    | e == clutterInitSuccess       = "CLUTTER_INIT_SUCCESS"
    | e == clutterInitErrorUnknown  = "CLUTTER_INIT_UNKNOWN"
    | e == clutterInitErrorThreads  = "CLUTTER_INIT_THREADS"
    | e == clutterInitErrorBackend  = "CLUTTER_INIT_BACKEND"
    | e == clutterInitErrorInternal = "CLUTTER_INIT_INTERNAL"
    | otherwise                     = "CLUTTER_INIT_UNKNOWN_ERROR"

#enum ClutterInitError, CIE\
  , CLUTTER_INIT_SUCCESS\
  , CLUTTER_INIT_ERROR_UNKNOWN\
  , CLUTTER_INIT_ERROR_THREADS\
  , CLUTTER_INIT_ERROR_BACKEND\
  , CLUTTER_INIT_ERROR_INTERNAL

withCArgs :: (Ptr CInt -> Ptr (Ptr CString) -> IO a) -> IO (a,[String])
withCArgs k = do
  args <- getArgs
  prog <- getProgName
  let loop (a:as) n ptrs = withCString a (\p -> loop as (n+1) (p:ptrs))
      loop [] n ptrs     =
        withArray (reverse ptrs) $ \ arr      ->
        with n                   $ \ argcAddr ->
        with arr                 $ \ argvAddr -> do
          res   <- k argcAddr argvAddr
          argc  <- peek argcAddr
          argv  <- peek argvAddr
          cstrs <- peekArray (fromIntegral argc) argv
          args' <- mapM peekCString cstrs
          return (res,tail args')
  loop (prog:args) 0 []


application :: IO () -> IO ()
application m =
  do (res,args) <- withCArgs c_clutter_init
     if res == clutterInitSuccess
       then withArgs args (m >> c_clutter_main)
       else throwIO res



foreign import ccall "clutter_init"
  c_clutter_init :: Ptr CInt -> Ptr (Ptr CString) -> IO ClutterInitError

foreign import ccall "clutter_main"
  c_clutter_main :: IO ()


