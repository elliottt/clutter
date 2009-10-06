module Clutter.Color where

import Data.Word
import Foreign.Storable

#include <clutter/clutter.h>

data Color = Color { red   :: !Word8
                   , green :: !Word8
                   , blue  :: !Word8
                   , alpha :: !Word8
                   }

defaultColor :: Color
defaultColor = Color 0 0 0 0

instance Storable Color where
  sizeOf _    = 4
  alignment _ = 4
  peek p      = do r <- (#peek ClutterColor, red)   p
                   g <- (#peek ClutterColor, green) p
                   b <- (#peek ClutterColor, blue)  p
                   a <- (#peek ClutterColor, alpha) p
                   return $ Color r g b a
  poke p c    = do (#poke ClutterColor, red)    p (red c)
                   (#poke ClutterColor, green)  p (green c)
                   (#poke ClutterColor, blue)   p (blue c)
                   (#poke ClutterColor, alpha)  p (alpha c)

class HasColor t where
  setColor :: t -> Color -> IO ()
  getColor :: t -> IO Color

