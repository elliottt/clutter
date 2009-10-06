{-# LANGUAGE ForeignFunctionInterface #-}

#include <cogl/cogl.h>

module Cogl.Prim (
    -- * Path Interface
    startPath
  , endPath
  , moveTo
  , moveRel
  , lineTo
  , lineRel
  , curveTo
  , curveRel
  , arcTo
  , line
  , polyline
  , polygon
  , rectangle
  , roundRectangle
  , ellipse
  , fill
  , fillPreserve
  , stroke
  , strokePreserve
  , fillRectangle
  ) where

import Foreign
import Foreign.C.Types

foreign import ccall "cogl_rectangle"
  fillRectangle :: Float -> Float -> Float -> Float -> IO ()

-- | Clears the current path and starts a new one.
foreign import ccall "cogl_path_new"
  startPath :: IO ()

-- | Closes the path being constructed by adding a straight line segment to it
-- that ends at the first vertex of the path.
foreign import ccall "cogl_path_close"
  endPath :: IO ()

-- | Moves the pen to the given location.  If there is an existing path, this
-- will start a new disjoint subpath.
foreign import ccall "cogl_path_move_to"
  moveTo :: Float -> Float -> IO ()

-- | Add a straight line segment that ends at the given coordinates.
foreign import ccall "cogl_path_line_to"
  lineTo :: Float -> Float -> IO ()

-- | Adds a cubic bezier curve segment to the current path.  A straight line
-- segment will link the current pen location to the first vertex of the arc.
-- If you perform a @moveTo@, the arcs start, just before drawing it, you get a
-- free-standing arc.
foreign import ccall "cogl_path_curve_to"
  curveTo :: Float -- ^ X-coordinate of the second control point
          -> Float -- ^ Y-coordinate of the second control point
          -> Float -- ^ X-coordinate of the third control point
          -> Float -- ^ Y-coordinate of the third control point
          -> Float -- ^ X-coordinate of the fourth control point
          -> Float -- ^ Y-coordinate of the fourth control point
          -> IO ()

-- | Adds an elliptical arc segment to the current path.  A straight line
-- segment will link the current pen location with the first vertex of the arc.
-- If you perform a @moveTo@ just before the arc, you will create a
-- free-standing arc.
foreign import ccall "cogl_path_arc"
  arcTo :: Float -- ^ X-coordinate to the elliptical arc center
        -> Float -- ^ Y-coordinate to the elliptical arc center
        -> Float -- ^ X-radius of the elliptical arc
        -> Float -- ^ Y-radius of the elliptical arc
        -> Float -- ^ Angle, in the unit circle, at which the arc begins
        -> Float -- ^ Angle, in the unit circle, at which the arc ends
        -> IO ()

-- | Moves the pen relative to given offset, relative to the current position.
-- If there is an existing path, this will start a disjoint subpath.
foreign import ccall "cogl_path_rel_move_to"
  moveRel :: Float -> Float -> IO ()

-- | Add a straight line segment that ends at the given coordinates, relative to
-- the current pen position.
foreign import ccall "cogl_path_rel_line_to"
  lineRel :: Float -> Float -> IO ()

-- | Adds a cubic bezier curve segment to the current path with the given
-- second, third and fourth control points, using the current pen position as
-- the first control point.  The given coordinates are relative to the current
-- pen position.
foreign import ccall "cogl_path_rel_curve_to"
  curveRel :: Float -- ^ X-coordinate of the second control point
           -> Float -- ^ Y-coordinate of the second control point
           -> Float -- ^ X-coordinate of the third control point
           -> Float -- ^ Y-coordinate of the third control point
           -> Float -- ^ X-coordinate of the fourth control point
           -> Float -- ^ Y-coordinate of the fourth control point
           -> IO ()

-- | Constructs a straight line shape starting and ending at the given
-- coordinates.  If there is an existing path, this will start a new, disjoint
-- subpath.
foreign import ccall "cogl_path_line"
  line :: Float -- ^ X-coordinate of the start line vertex
       -> Float -- ^ Y-coordinate of the start line vertex
       -> Float -- ^ X-coordinate of the end line vertex
       -> Float -- ^ Y-coordinate of the end line vertex
       -> IO ()

withCoordList :: [(Float,Float)] -> (Ptr Float -> CInt -> IO a) -> IO a
withCoordList coords f = loop [] 0 coords
  where
  loop ps n ((x,y):cs) = loop (y:x:ps) (n+2) cs
  loop ps n []         = withArray (reverse ps) (\ptr -> f ptr n)

foreign import ccall "cogl_path_polyline"
  cogl_path_polyline :: Ptr Float -> CInt -> IO ()

-- | Constructs a series of straight line segments, starting from the first
-- vertex coordinate.  If there is an existing path, this will start a new,
-- disjoint subpath.  Each subsequent starts where the previous one ended, and
-- ends at the next given vertex coordinate.
polyline :: [(Float,Float)] -> IO ()
polyline coords = withCoordList coords cogl_path_polyline

foreign import ccall "cogl_path_polygon"
  cogl_path_polygon :: Ptr Float -> CInt -> IO ()

-- | Constructs a polygon out of the given vertices.  If there is an existing
-- path, this will start a new, disjoint subpath.
polygon :: [(Float,Float)] -> IO ()
polygon coords = withCoordList coords cogl_path_polygon

-- | Constructs a rectangular shape at the given coordinates.  If there is an
-- existing path, this will start a new, disjoint subpath.
foreign import ccall "cogl_path_rectangle"
  rectangle :: Float -> Float -> Float -> Float -> IO ()

-- | Constructs a rectangular shape with rounded corners.  If there is an
-- existing path, this will start a new, disjoint subpath.
foreign import ccall "cogl_path_round_rectangle"
  roundRectangle :: Float -- ^ X-coordinate of the top-left corner
                 -> Float -- ^ Y-coordinate of the top-left corner
                 -> Float -- ^ X-coordinate of the bottom-right corner
                 -> Float -- ^ Y-coordinate of the bottom-right corner
                 -> Float -- ^ Radius of the corner arcs
                 -> Float -- ^ Angle increment resolution for subdivision of the
                          --   corner arcs
                 -> IO ()

-- | Constructs an ellipse shape.  If there is an existing ptah, this will start
-- a new, disjoint subpath.
foreign import ccall "cogl_path_ellipse"
  ellipse :: Float -- ^ X-coordinate of the ellipse center
          -> Float -- ^ Y-coordinate of the ellipse center
          -> Float -- ^ X-radius of the ellipse
          -> Float -- ^ Y-radius of the ellipse
          -> IO ()

-- | Fills the constructed shape using the current drawing color.  The current
-- path is then cleared.  To preserve the path, use @fillPreserve@.
foreign import ccall "cogl_path_fill"
  fill :: IO ()

-- | Fills the constructed shape using the current drawing color, and preserves
-- the path to be used again.
foreign import ccall "cogl_path_fill_preserve"
  fillPreserve :: IO ()

-- | Strokes the constructed shape using the current drawing color, and a width
-- of 1 pixel (regardless of the current transformation matrix).  The current
-- path is then cleared.  To preserve the path, use @strokePreserve@.
foreign import ccall "cogl_path_stroke"
  stroke :: IO ()

-- | Strokes the constructed shape using the current drawing color, and
-- preserves the path to be used again.
foreign import ccall "cogl_path_stroke_preserve"
  strokePreserve :: IO ()
