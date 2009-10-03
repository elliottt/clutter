module Clutter.Private where

import Foreign.Ptr(Ptr)

-- | A mouse button event.
newtype ButtonEvent = BE { unBE :: Ptr () }

-- | Types that are clutter actors.
class Actor t where
  fromActor :: t -> Ptr ()




