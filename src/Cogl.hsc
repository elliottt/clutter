{-# LANGUAGE ForeignFunctionInterface #-}

#include <cogl/cogl.h>

module Cogl (
    -- * Matrix Operations
    pushMatrix
  , popMatrix
  , scale
  , translate
  , rotate
  , frustum
  , perspective
  , ortho
  , clear

  , CoglBufferBit
  , bufferBitColor
  , bufferBitDepth
  , bufferBitStencil

  , setSource
  , setSourceColor
  , setSourceColor4b
  , setSourceColor4f
  , setSourceTexture

  , module Cogl.Color
  , module Cogl.Material
  , module Cogl.Prim
  , module Cogl.Texture
  ) where

import Cogl.Color
import Cogl.Material
import Cogl.Prim
import Cogl.Texture

import Data.Bits (Bits((.|.)))
import Data.Word (Word8)
import Foreign.C.Types

-- | Store the current model-view matrix from the matrix stack.  The matrix can
-- later be restored with @popMatrix@.
foreign import ccall "cogl_push_matrix"
  pushMatrix :: IO ()

-- | Restore the current model-view matrix from the matrix stack.
foreign import ccall "cogl_pop_matrix"
  popMatrix :: IO ()

-- | Multiplies the current model-view matrix by one that scales the x y and z
-- axes by the given values.
foreign import ccall "cogl_scale"
  scale :: Float -- ^ Amount to scale along the x-axis
        -> Float -- ^ Amount to scale along the y-axis
        -> Float -- ^ Amount to scale along the z-axis
        -> IO ()

-- | Multiplies the current model-view matrix by one that translates the model
-- along all three axes according to the given values.
foreign import ccall "cogl_translate"
  translate :: Float -- ^ Distance to translate along the x-axis
            -> Float -- ^ Distance to translate along the y-axis
            -> Float -- ^ Distance to translate along the z-axis
            -> IO ()

-- | Multiplies the current model-view matrix by one that rotates the model
-- around the vertex specified by x, y and z.  The rotation follows the
-- right-hand rule.  For example, rotating by 10 degrees about the vertex
-- (0,0,1) causes a small counter-clockwise rotation.
foreign import ccall "cogl_rotate"
  rotate :: Float -- ^ Angle in degrees to rotate
         -> Float -- ^ X-component of vertex to rotate about
         -> Float -- ^ Y-component of vertex to rotate about
         -> Float -- ^ Z-component of vertex to rotate about
         -> IO ()

foreign import ccall "cogl_frustum"
  frustum :: Float -- ^ Left clipping plane
          -> Float -- ^ Right clipping plane
          -> Float -- ^ Bottom clipping plane
          -> Float -- ^ Top clipping plane
          -> Float -- ^ Nearest visible point
          -> Float -- ^ Furthest visible point along the z-axis
          -> IO ()

-- | Replaces the current projection matrix with a perspective matrix based on
-- the provided values.
foreign import ccall "cogl_perspective"
  perspective :: Float -- ^ Vertical field of view in degrees
              -> Float -- ^ Aspect ratio of display
              -> Float -- ^ Nearest visible point
              -> Float -- ^ Furthest visible point along the z-axis
              -> IO ()

-- | Replaces the current projection matrix with a parallel projection matrix.
foreign import ccall "cogl_ortho"
  ortho :: Float -- ^ The coordinate for the left clipping plane
        -> Float -- ^ The coordinate for the right clipping plane
        -> Float -- ^ The coordinate for the bottom clipping plane
        -> Float -- ^ The coordinate for the top clipping plane
        -> Float -- ^ The coordinate for the near clipping plane
        -> Float -- ^ The corrdinate for the far clipping plane
        -> IO ()

newtype CoglBufferBit = CBB CULong

#enum CoglBufferBit, CBB\
  , bufferBitColor   = COGL_BUFFER_BIT_COLOR\
  , bufferBitDepth   = COGL_BUFFER_BIT_DEPTH\
  , bufferBitStencil = COGL_BUFFER_BIT_STENCIL

foreign import ccall "cogl_clear"
  cogl_clear :: CoglColor -> CULong -> IO ()

clear :: CoglColor -> [CoglBufferBit] -> IO ()
clear c bs = cogl_clear c mask
  where mask = foldl (\z (CBB a) -> z .|. a) 0 bs

foreign import ccall "cogl_set_source"
  setSource :: CoglHandle -> IO ()

foreign import ccall "cogl_set_source_color"
  setSourceColor :: CoglColor -> IO ()

foreign import ccall "cogl_set_source_color4ub"
  setSourceColor4b :: Word8 -> Word8 -> Word8 -> Word8 -> IO ()

foreign import ccall "cogl_set_source_color4f"
  setSourceColor4f :: Float -> Float -> Float -> Float -> IO ()

foreign import ccall "cogl_set_source_texture"
  setSourceTexture :: CoglHandle -> IO ()
