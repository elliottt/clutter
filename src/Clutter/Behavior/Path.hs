module Clutter.Behavior.Path
  ( PathBehavior
  , newPathBehavior
  , Alpha
  , Path
  ) where

import Clutter.Private

import Foreign.Ptr
import Foreign.ForeignPtr

newtype PathBehavior = PB (ForeignPtr ())
instance ForeignObject PathBehavior where rawPtr (PB p) = p
instance Behavior PathBehavior

newPathBehavior :: Alpha -> Path -> IO PathBehavior
newPathBehavior a p = withAlpha a $ \ap ->
                      withPath  p $ \pp ->
  do pr <- clutter_behaviour_path_new ap pp
     fp <- newGObject pr
     return (PB fp)

--------------------------------------------------------------------------------

foreign import ccall "clutter_behaviour_path_new"
  clutter_behaviour_path_new :: Ptr () -> Ptr () -> IO (Ptr ())


