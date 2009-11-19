module Clutter.Alpha
  ( Alpha
  , newAlpha
  , AnimationMode(..)
  , mCustom
  , mLinear
  , mEaseIn2 
  , mEaseOut2
  , mEaseInOut2
  , mEaseIn3
  , mEaseOut3
  , mEaseInOut3
  , mEaseIn4
  , mEaseOut4
  , mEaseInOut4
  , mEaseIn5
  , mEaseOut5
  , mEaseInOut5
  , mEaseInSine
  , mEaseOutSine
  , mEaseInOutSine
  , mEaseInExpo
  , mEaseOutExpo
  , mEaseInOutExpo
  , mEaseInCirc
  , mEaseOutCirc
  , mEaseInOutCirc
  , mEaseInElastic
  , mEaseOutElastic
  , mEaseInOutElastic
  , mEaseInBack
  , mEaseOutBack
  , mEaseInOutBack
  , mEaseInBounce
  , mEaseOutBounce
  , mEaseInOutBounce
  , mLast
  ) where

import Clutter.Private

import Foreign.Ptr


newAlpha :: Timeline -> AnimationMode -> IO Alpha
newAlpha t x = withTimeline t $ \pt ->
  ptrAlpha =<< clutter_alpha_new_full pt x

--------------------------------------------------------------------------------

foreign import ccall "clutter_alpha_new_full"
  clutter_alpha_new_full :: Ptr () -> AnimationMode -> IO (Ptr ())


