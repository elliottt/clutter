module Clutter.Path
  ( Path
  , Point
  , newPath
  , move
  , relMove
  , line
  , relLine
  , curve
  , relCurve
  , close
  ) where

import Clutter.Private

import Foreign.Ptr
import Foreign.C.Types

type Point          = (Int,Int)

newPath            :: IO Path
newPath             = ptrPath =<< clutter_path_new

move               :: Path -> Point -> IO ()
move p (x,y)        = withPath p $ \pp -> clutter_path_add_move_to pp
                                            (fromIntegral x) (fromIntegral y)

relMove            :: Path -> Point -> IO ()
relMove p (x,y)     = withPath p $ \pp -> clutter_path_add_rel_move_to pp
                                            (fromIntegral x) (fromIntegral y)

line               :: Path -> Point -> IO ()
line p (x,y)        = withPath p $ \pp -> clutter_path_add_line_to pp
                                            (fromIntegral x) (fromIntegral y)

relLine            :: Path -> Point -> IO ()
relLine p (x,y)     = withPath p $ \pp -> clutter_path_add_rel_line_to pp
                                            (fromIntegral x) (fromIntegral y)

curve              :: Path -> Point -> Point -> Point -> IO ()
curve p (x1,y1) (x2,y2) (x3,y3)
                    = withPath p $ \pp -> clutter_path_add_curve_to pp
                                            (fromIntegral x1) (fromIntegral y1)
                                            (fromIntegral x2) (fromIntegral y2)
                                            (fromIntegral x3) (fromIntegral y3)

relCurve           :: Path -> Point -> Point -> Point -> IO ()
relCurve p (x1,y1) (x2,y2) (x3,y3)
                    = withPath p $ \pp -> clutter_path_add_rel_curve_to pp
                                            (fromIntegral x1) (fromIntegral y1)
                                            (fromIntegral x2) (fromIntegral y2)
                                            (fromIntegral x3) (fromIntegral y3)

close              :: Path -> IO ()
close p             = withPath p $ \pp -> clutter_path_add_close pp







--------------------------------------------------------------------------------
foreign import ccall "clutter_path_new"
  clutter_path_new                :: IO (Ptr ())

foreign import ccall "clutter_path_add_move_to"
  clutter_path_add_move_to        :: Ptr ()
                                  -> CInt -> CInt
                                  -> IO ()

foreign import ccall "clutter_path_add_rel_move_to"
  clutter_path_add_rel_move_to    :: Ptr ()
                                  -> CInt -> CInt
                                  -> IO ()

foreign import ccall "clutter_path_add_line_to"
  clutter_path_add_line_to        :: Ptr ()
                                  -> CInt -> CInt
                                  -> IO ()

foreign import ccall "clutter_path_add_rel_line_to"
  clutter_path_add_rel_line_to    :: Ptr ()
                                  -> CInt -> CInt
                                  -> IO ()

foreign import ccall "clutter_path_add_curve_to"
  clutter_path_add_curve_to       :: Ptr ()
                                  -> CInt -> CInt
                                  -> CInt -> CInt
                                  -> CInt -> CInt
                                  -> IO ()

foreign import ccall "clutter_path_add_rel_curve_to"
  clutter_path_add_rel_curve_to   :: Ptr ()
                                  -> CInt -> CInt
                                  -> CInt -> CInt
                                  -> CInt -> CInt
                                  -> IO ()

foreign import ccall "clutter_path_add_close"
  clutter_path_add_close          :: Ptr ()
                                  -> IO ()
