module Clutter.Private.Types
  ( RotateAxis
  , xAxis
  , yAxis
  , zAxis

  , RotateDirection
  , clockwise
  , counterClockwise

  ) where

#include <clutter/clutter.h>

import Data.Word

--------------------------------------------------------------------------------
newtype RotateAxis = RotateAxis (#type ClutterRotateAxis)
                      deriving (Eq)

#enum RotateAxis, RotateAxis  \
  , xAxis = CLUTTER_X_AXIS    \
  , yAxis = CLUTTER_Y_AXIS    \
  , zAxis = CLUTTER_Z_AXIS
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
newtype RotateDirection = RotateDirection (#type ClutterRotateDirection)
                            deriving (Eq)

#enum RotateDirection, RotateDirection    \
  , clockwise        = CLUTTER_ROTATE_CW  \
  , counterClockwise = CLUTTER_ROTATE_CCW
--------------------------------------------------------------------------------


