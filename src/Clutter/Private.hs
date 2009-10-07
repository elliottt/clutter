module Clutter.Private where

import Foreign.Ptr(Ptr)
import Foreign.ForeignPtr(ForeignPtr,withForeignPtr)
import Clutter.GLib

-- | A mouse button event.
newtype ButtonEvent = BE { unBE :: Ptr () }

class ForeignObject t where
  rawPtr :: t -> ForeignPtr ()

-- | Types that are clutter actors.
class ForeignObject t => Actor t

-- | Types that are clutter containers.
class ForeignObject t => Container t

withPtr :: ForeignObject t => t -> (Ptr () -> IO a) -> IO a
withPtr p k = withForeignPtr (rawPtr p) k

withActor :: Actor a => a -> (Ptr () -> IO b) -> IO b
withActor = withPtr

withContainer :: Container c => c -> (Ptr () -> IO b) -> IO b
withContainer = withPtr


newtype SomeActor = SomeActor (ForeignPtr ())

someActor :: Ptr () -> IO SomeActor
someActor p = SomeActor `fmap` newGObject p

instance ForeignObject SomeActor where rawPtr (SomeActor p) = p 
instance Actor SomeActor

