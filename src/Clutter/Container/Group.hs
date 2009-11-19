module Clutter.Container.Group
  ( Group
  , toGroup
  , newGroup
  , removeAll
  , childNum
  , getChild  
  ) where

import Foreign.Ptr(Ptr,nullPtr)
import Foreign.ForeignPtr(ForeignPtr)
import Foreign.C.Types
import Data.Word

import Clutter.Private


newtype Group = G (ForeignPtr ())

instance ForeignObject Group where rawPtr (G p) = p
instance Actor Group
instance Container Group

toGroup :: SomeActor -> IO (Maybe Group)
toGroup p = do t  <- withActor p getType
               ts <- clutter_group_get_type
               return (if t == ts then Just (G (rawPtr p)) else Nothing)

foreign import ccall "clutter_group_get_type"
  clutter_group_get_type :: IO GType
--------------------------------------------------------------------------------



foreign import ccall "clutter_group_new"
  clutter_group_new             :: IO (Ptr ())

foreign import ccall "clutter_group_remove_all"
  clutter_group_remove_all      :: Ptr () -> IO ()

foreign import ccall "clutter_group_get_n_children"
  clutter_group_get_n_children  :: Ptr () -> IO CInt

foreign import ccall "clutter_group_get_nth_child"
  clutter_group_get_nth_child   :: Ptr () -> CInt -> IO (Ptr ())


newGroup     :: IO Group
newGroup      = G `fmap` (newGObject =<< clutter_group_new)

removeAll    :: Group -> IO ()
removeAll g   = withPtr g clutter_group_remove_all

childNum     :: Group -> IO Word
childNum g    = fromIntegral `fmap` withPtr g clutter_group_get_n_children

getChild     :: Group -> Word -> IO (Maybe SomeActor)
getChild g n  = do p <- withPtr g $ \gp ->
                        clutter_group_get_nth_child gp (fromIntegral n)
                   if p == nullPtr then return Nothing
                                   else Just `fmap` someActor p



