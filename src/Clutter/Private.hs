module Clutter.Private where

import Foreign.Ptr(Ptr)
import Foreign.ForeignPtr(ForeignPtr,withForeignPtr)
import Clutter.GLib

-- | A mouse button event.
newtype ButtonEvent = BE { unBE :: Ptr () }


--------------------------------------------------------------------------------
class ForeignObject t where
  rawPtr :: t -> ForeignPtr ()

withPtr :: ForeignObject t => t -> (Ptr () -> IO a) -> IO a
withPtr p k = withForeignPtr (rawPtr p) k


--------------------------------------------------------------------------------
-- | Types that are clutter actors.
class ForeignObject t => Actor t

withActor        :: Actor a => a -> (Ptr () -> IO b) -> IO b
withActor         = withPtr

newtype SomeActor = SomeActor     (ForeignPtr ())

someActor        :: Ptr () -> IO SomeActor
someActor p       = SomeActor `fmap` newGObject p

instance ForeignObject SomeActor where rawPtr (SomeActor p) = p 
instance Actor SomeActor


--------------------------------------------------------------------------------
-- | Types that are clutter containers.
class ForeignObject t => Container t

withContainer        :: Container a => a -> (Ptr () -> IO b) -> IO b
withContainer         = withPtr

newtype SomeContainer = SomeContainer     (ForeignPtr ())

someContainer        :: Ptr () -> IO SomeContainer
someContainer p       = SomeContainer `fmap` newGObject p

instance ForeignObject SomeContainer where rawPtr (SomeContainer p) = p 
instance Container SomeContainer


--------------------------------------------------------------------------------
-- | Types that are clutter containers.
class ForeignObject t => Behavior t

withBehavior        :: Behavior a => a -> (Ptr () -> IO b) -> IO b
withBehavior         = withPtr

newtype SomeBehavior = SomeBehavior     (ForeignPtr ())

someBehavior        :: Ptr () -> IO SomeBehavior
someBehavior p       = SomeBehavior `fmap` newGObject p

instance ForeignObject SomeBehavior where rawPtr (SomeBehavior p) = p 
instance Behavior SomeBehavior


--------------------------------------------------------------------------------

newtype Timeline    = Timeline (ForeignPtr ())

instance ForeignObject Timeline where
  rawPtr (Timeline t) = t

ptrTimeline        :: Ptr () -> IO Timeline
ptrTimeline p       = Timeline `fmap` newGObject p

withTimeline       :: Timeline -> (Ptr () -> IO a) -> IO a
withTimeline        = withPtr


--------------------------------------------------------------------------------
newtype Alpha       = Alpha (ForeignPtr ())

instance ForeignObject Alpha where
  rawPtr (Alpha p) = p

ptrAlpha           :: Ptr () -> IO Alpha
ptrAlpha p          = Alpha `fmap` newGObject p

withAlpha          :: Alpha -> (Ptr () -> IO a) -> IO a
withAlpha           = withPtr


--------------------------------------------------------------------------------
newtype Path        = Path (ForeignPtr ())

instance ForeignObject Path where
  rawPtr (Path p) = p

ptrPath            :: Ptr () -> IO Path
ptrPath p           = Path `fmap` newGObject p

withPath           :: Path -> (Ptr () -> IO a) -> IO a
withPath            = withPtr


