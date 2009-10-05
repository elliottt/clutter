module Clutter.Private where

import Foreign.Ptr(Ptr)

-- | A mouse button event.
newtype ButtonEvent = BE { unBE :: Ptr () }

-- | Types that are clutter actors.
class Actor t where
  withActor :: t -> (Ptr () -> IO a) -> IO a

-- | Types that are clutter containers.
class Container t where
  fromContainer :: t -> Ptr ()


