module Clutter.AnimationMode
  ( AnimationMode(..)
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

#include <clutter/clutter.h>

import Foreign.C.Types

newtype AnimationMode = AnimationMode CULong

#enum AnimationMode, AnimationMode, \
  mCustom = CLUTTER_CUSTOM_MODE, \
  mLinear = CLUTTER_LINEAR, \
  mEaseIn2    = CLUTTER_EASE_IN_QUAD, \
  mEaseOut2   = CLUTTER_EASE_OUT_QUAD, \
  mEaseInOut2 = CLUTTER_EASE_IN_OUT_QUAD, \
  mEaseIn3    = CLUTTER_EASE_IN_CUBIC, \
  mEaseOut3   = CLUTTER_EASE_OUT_CUBIC, \
  mEaseInOut3 = CLUTTER_EASE_IN_OUT_CUBIC, \
  mEaseIn4    = CLUTTER_EASE_IN_QUART, \
  mEaseOut4   = CLUTTER_EASE_OUT_QUART, \
  mEaseInOut4 = CLUTTER_EASE_IN_OUT_QUART, \
  mEaseIn5    = CLUTTER_EASE_IN_QUINT, \
  mEaseOut5   = CLUTTER_EASE_OUT_QUINT, \
  mEaseInOut5 = CLUTTER_EASE_IN_OUT_QUINT, \
  mEaseInSine    = CLUTTER_EASE_IN_SINE, \
  mEaseOutSine   = CLUTTER_EASE_OUT_SINE, \
  mEaseInOutSine = CLUTTER_EASE_IN_OUT_SINE, \
  mEaseInExpo    = CLUTTER_EASE_IN_EXPO, \
  mEaseOutExpo   = CLUTTER_EASE_OUT_EXPO, \
  mEaseInOutExpo = CLUTTER_EASE_IN_OUT_EXPO, \
  mEaseInCirc    = CLUTTER_EASE_IN_CIRC, \
  mEaseOutCirc   = CLUTTER_EASE_OUT_CIRC, \
  mEaseInOutCirc = CLUTTER_EASE_IN_OUT_CIRC, \
  mEaseInElastic    = CLUTTER_EASE_IN_ELASTIC, \
  mEaseOutElastic   = CLUTTER_EASE_OUT_ELASTIC, \
  mEaseInOutElastic = CLUTTER_EASE_IN_OUT_ELASTIC, \
  mEaseInBack    = CLUTTER_EASE_IN_BACK, \
  mEaseOutBack   = CLUTTER_EASE_OUT_BACK, \
  mEaseInOutBack = CLUTTER_EASE_IN_OUT_BACK, \
  mEaseInBounce    = CLUTTER_EASE_IN_BOUNCE, \
  mEaseOutBounce   = CLUTTER_EASE_OUT_BOUNCE, \
  mEaseInOutBounce = CLUTTER_EASE_IN_OUT_BOUNCE, \
  mLast            = CLUTTER_ANIMATION_LAST


