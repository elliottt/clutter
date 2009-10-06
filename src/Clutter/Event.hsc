module Clutter.Event
  ( ButtonEvent
  , btnX
  , btnY
  , btnTime
  , btnClicks
  , btnButton
  , btnModifiers
  , Modifiers
  , mShift, mLock, mControl
  , mMod1, mMod2, mMod3, mMod4, mMod5
  , mButton1, mButton2, mButton3, mButton4, mButton5
  , mSuper, mHyper, mMeta
  , mRelease
  , subset, union
  ) where

import Foreign.C.Types
import Foreign.Storable(peekByteOff)
import Data.Word
import Data.Bits

import Clutter.Private

#include <clutter/clutter.h>

-- | Horizontal coordinate of event, relative to stage.
btnX           :: ButtonEvent -> IO Float
btnX e          = (#peek ClutterButtonEvent, x) (unBE e)

-- | Vertical coordinate of event, relative to stage.
btnY           :: ButtonEvent -> IO Float
btnY e          = (#peek ClutterButtonEvent, y) (unBE e)

-- | Time of the event.
btnTime        :: ButtonEvent -> IO Word
btnTime e       = do x <- (#peek ClutterButtonEvent, click_count) (unBE e)
                     return (fromIntegral (x :: Word32))

-- | Number of button presses within the default time and radius.
btnClicks      :: ButtonEvent -> IO Word
btnClicks e     = do x <- (#peek ClutterButtonEvent, click_count) (unBE e)
                     return (fromIntegral (x :: CUInt))

-- | Which button was pressed.
btnButton      :: ButtonEvent -> IO Word
btnButton e     = do x <- (#peek ClutterButtonEvent, button) (unBE e)
                     return (fromIntegral (x :: Word32))

-- | What modifiers were pressed when the event occured.
btnModifiers   :: ButtonEvent -> IO Modifiers
btnModifiers e  = do x <- (#peek ClutterButtonEvent, modifier_state) (unBE e)
                     return (M x)

-- | A set of modifiers.
newtype Modifiers = M CInt

-- | Check if all modifiers in the first set are also present in the second.
subset :: Modifiers -> Modifiers -> Bool
subset (M x) (M y) = (x .&. y) /= 0

-- | The union of two sets of modifiers.
union :: Modifiers -> Modifiers -> Modifiers
union (M x) (M y) = M (x .|. y)



#enum Modifiers, M\
  , mShift   = CLUTTER_SHIFT_MASK\
  , mLock    = CLUTTER_LOCK_MASK\
  , mControl = CLUTTER_CONTROL_MASK\
  , mMod1    = CLUTTER_MOD1_MASK\
  , mMod2    = CLUTTER_MOD2_MASK\
  , mMod3    = CLUTTER_MOD3_MASK\
  , mMod4    = CLUTTER_MOD4_MASK\
  , mMod5    = CLUTTER_MOD5_MASK\
  , mButton1 = CLUTTER_BUTTON1_MASK\
  , mButton2 = CLUTTER_BUTTON2_MASK\
  , mButton3 = CLUTTER_BUTTON3_MASK\
  , mButton4 = CLUTTER_BUTTON4_MASK\
  , mButton5 = CLUTTER_BUTTON5_MASK\
  , mSuper   = CLUTTER_SUPER_MASK\
  , mHyper   = CLUTTER_HYPER_MASK\
  , mMeta    = CLUTTER_META_MASK\
  , mRelease = CLUTTER_RELEASE_MASK

instance Show Modifiers where
  show m = "{" ++ jn [ y | (x,y) <- how, x `subset` m ] ++ "}"
    where
    jn []     = ""
    jn [x]    = x
    jn (x:xs) = x ++ concatMap (", " ++) xs

    how = [ (mShift, "shift")
          , (mLock, "lock")
          , (mControl, "control")
          , (mMod1, "mod 1")
          , (mMod2, "mod 2")
          , (mMod3, "mod 3")
          , (mMod4, "mod 4")
          , (mMod5, "mod 5")
          , (mButton1, "button 1")
          , (mButton2, "button 2")
          , (mButton3, "button 3")
          , (mButton4, "button 4")
          , (mButton5, "button 5")

          , (mSuper, "super")
          , (mHyper, "hyper")
          , (mMeta,  "meta")

          , (mRelease, "release")
          ]


