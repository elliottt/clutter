{-# LANGUAGE ForeignFunctionInterface #-}
module Clutter.Actor.Text
  ( 
    -- ** General
    Text
  , toText
  , Font(..)

  -- ** Constructros
  , newText
  , newTextWithColor

  -- ** Properties

  -- *** Text Manipulation 
  , setText
  , insertText
  , appendText
  , deleteChars
  , deleteText
  , getText
  , getChars
  , setCursorPosition
  , getCursorPosition

 

  -- *** Behavior
  , setEditable
  , getEditable

  , setActivatable
  , getActivatable

  , setSelectable
  , getSelectable

  , setLineWrap
  , getLineWrap

  , setSingleLine
  , getSingleLine

  , setMaxLength
  , setNoMaxLength
  , setMaxLengthCur
  , getMaxLength

  , setCursorVisible 
  , getCursorVisible 


  -- *** Selection
  , setSelection
  , setSelectionBound  
  , clearSelection
  , getSelection
  , getSelectionBound
  , deleteSelection

  -- *** Looks
  , setFont
  , getFont

  , setPassword
  , setNotPassword
  , getPasswordChar

  , setCursorColor
  , resetCursorColor
  , getCursorColor

  , setSelectionColor
  , resetSelectionColor
  , getSelectionColor

  , setCursorSize
  , resetCursorSize
  , getCursorSize

  -- ** Signals
  , onActivate
  , onTextChanged
  ) where

import Clutter.Color
import Clutter.Private

import Foreign hiding (void)
import Foreign.C

--------------------------------------------------------------------------------
newtype Text = T (ForeignPtr ())

instance ForeignObject Text where
  rawPtr (T p) = p

instance Actor Text

toText :: SomeActor -> IO (Maybe Text)
toText p = do t  <- withActor p getType
              ts <- clutter_text_get_type
              return (if t == ts then Just (T (rawPtr p)) else Nothing)

foreign import ccall "clutter_text_get_type"
  clutter_text_get_type :: IO GType
--------------------------------------------------------------------------------

newtype Font = Font String

foreign import ccall "clutter_text_new_full"
  clutter_text_new_full :: CString -> CString -> Ptr Color -> IO (Ptr ())

newTextWithColor :: Font -> String -> Color -> IO Text
newTextWithColor (Font f) s c =
  withCString f $ \fp ->
  withCString s $ \sp ->
  with c        $ \cp ->
    T `fmap` (newGObject =<< clutter_text_new_full fp sp cp)

foreign import ccall "clutter_text_new_with_text"
  clutter_text_new_with_text :: CString -> CString -> IO (Ptr ())

newText :: Font -> String -> IO Text
newText (Font f) s =
  withCString f $ \fp ->
  withCString s $ \sp ->
    T `fmap` (newGObject =<< clutter_text_new_with_text fp sp)


--------------------------------------------------------------------------------

instance HasColor Text where
  setColor r c = withPtr r $ \p -> with c (clutter_text_set_color p)
  getColor r   = with defaultColor $ \c ->
                 withPtr r $ \p ->
                   do clutter_text_get_color p c
                      peek c

foreign import ccall "clutter_text_get_color"
  clutter_text_get_color :: Ptr () -> Ptr Color -> IO ()

foreign import ccall "clutter_text_set_color"
  clutter_text_set_color :: Ptr () -> Ptr Color -> IO ()



foreign import ccall "clutter_text_set_editable"
  clutter_text_set_editable :: Ptr () -> Bool -> IO ()

foreign import ccall "clutter_text_get_editable"
  clutter_text_get_editable :: Ptr () -> IO Bool

setEditable :: Text -> Bool -> IO ()
setEditable t b = withPtr t $ \p -> clutter_text_set_editable p b

getEditable :: Text -> IO Bool
getEditable t = withPtr t $ \p -> clutter_text_get_editable p



foreign import ccall "clutter_text_set_activatable"
  clutter_text_set_activatable :: Ptr () -> Bool -> IO ()

foreign import ccall "clutter_text_get_activatable"
  clutter_text_get_activatable :: Ptr () -> IO Bool

setActivatable :: Text -> Bool -> IO ()
setActivatable t b = withPtr t $ \p -> clutter_text_set_activatable p b

getActivatable :: Text -> IO Bool
getActivatable t = withPtr t $ \p -> clutter_text_get_activatable p



foreign import ccall "clutter_text_set_text"
  clutter_text_set_text :: Ptr () -> CString -> IO ()

foreign import ccall "clutter_text_get_text"
  clutter_text_get_text :: Ptr () -> IO CString

setText :: Text -> String -> IO ()
setText t s = withPtr t     $ \pt ->
              withCString s $ \ps -> clutter_text_set_text pt ps

getText :: Text -> IO String
getText t = withPtr t $ \p -> peekCString =<< clutter_text_get_text p



foreign import ccall "clutter_text_set_font_name"
  clutter_text_set_font_name :: Ptr () -> CString -> IO ()

foreign import ccall "clutter_text_get_font_name"
  clutter_text_get_font_name :: Ptr () -> IO CString

setFont :: Text -> Font -> IO ()
setFont t (Font s) = withPtr t     $ \pt ->
                         withCString s $ \ps ->
                         clutter_text_set_font_name pt ps

getFont :: Text -> IO Font
getFont t = withPtr t $ \p ->
            Font `fmap` (peekCString =<< clutter_text_get_font_name p)



foreign import ccall "clutter_text_set_password_char"
  clutter_text_set_password_char :: Ptr () -> Word32 -> IO ()

foreign import ccall "clutter_text_get_password_char"
  clutter_text_get_password_char :: Ptr () -> IO Word32

setPassword :: Text -> Char -> IO ()
setPassword t c =
  withPtr t $ \pt ->
  clutter_text_set_password_char pt (fromIntegral (fromEnum c))

setNotPassword :: Text -> IO ()
setNotPassword t =
  withPtr t $ \pt -> clutter_text_set_password_char pt 0



getPasswordChar :: Text -> IO Char
getPasswordChar t = withPtr t $ \p ->
            (toEnum . fromIntegral) `fmap` clutter_text_get_password_char p





foreign import ccall "clutter_text_set_line_wrap"
  clutter_text_set_line_wrap :: Ptr () -> Bool -> IO ()

foreign import ccall "clutter_text_get_line_wrap"
  clutter_text_get_line_wrap :: Ptr () -> IO Bool

setLineWrap :: Text -> Bool -> IO ()
setLineWrap t b = withPtr t $ \p -> clutter_text_set_line_wrap p b

getLineWrap :: Text -> IO Bool
getLineWrap t = withPtr t $ \p -> clutter_text_get_line_wrap p



foreign import ccall "clutter_text_set_max_length"
  clutter_text_set_max_length :: Ptr () -> CInt -> IO ()

foreign import ccall "clutter_text_get_max_length"
  clutter_text_get_max_length :: Ptr () -> IO CInt

setMaxLength :: Text -> Word -> IO ()
setMaxLength t b = withPtr t $ \p ->
                   clutter_text_set_max_length p (fromIntegral b)

setNoMaxLength :: Text -> IO ()
setNoMaxLength t = withPtr t $ \p ->
                     clutter_text_set_max_length p 0

setMaxLengthCur :: Text -> IO ()
setMaxLengthCur t = withPtr t $ \p ->
                    clutter_text_set_max_length p (-1)

getMaxLength :: Text -> IO Word
getMaxLength t = withPtr t $ \p ->
                 fromIntegral `fmap`clutter_text_get_max_length p




foreign import ccall "clutter_text_set_selectable"
  clutter_text_set_selectable :: Ptr () -> Bool -> IO ()

foreign import ccall "clutter_text_get_selectable"
  clutter_text_get_selectable :: Ptr () -> IO Bool

setSelectable :: Text -> Bool -> IO ()
setSelectable t b = withPtr t $ \p -> clutter_text_set_selectable p b

getSelectable :: Text -> IO Bool
getSelectable t = withPtr t $ \p -> clutter_text_get_selectable p



foreign import ccall "clutter_text_set_selection"
  clutter_text_set_selection :: Ptr () -> CInt -> CInt -> IO ()

foreign import ccall "clutter_text_get_selection"
  clutter_text_get_selection :: Ptr () -> IO CString

setSelection :: Text -> Int -> Int -> IO ()
setSelection t x y  =
  withPtr t $ \p ->
  clutter_text_set_selection p (fromIntegral x) (fromIntegral y)

getSelection :: Text -> IO String
getSelection t = withPtr t $ \p -> peekCString =<< clutter_text_get_selection p




foreign import ccall "clutter_text_set_selection_bound"
  clutter_text_set_selection_bound :: Ptr () -> CInt -> IO ()

foreign import ccall "clutter_text_get_selection_bound"
  clutter_text_get_selection_bound :: Ptr () -> IO CInt

setSelectionBound :: Text -> Word -> IO ()
setSelectionBound t b = withPtr t $ \p ->
                        clutter_text_set_selection_bound p (fromIntegral b)

clearSelection :: Text -> IO ()
clearSelection t = withPtr t $ \p ->
                   clutter_text_set_selection_bound p (-1)

getSelectionBound :: Text -> IO Int
getSelectionBound t = withPtr t $ \p ->
                      fromIntegral `fmap`clutter_text_get_selection_bound p



foreign import ccall "clutter_text_set_single_line_mode"
  clutter_text_set_single_line_mode :: Ptr () -> Bool -> IO ()

foreign import ccall "clutter_text_get_single_line_mode"
  clutter_text_get_single_line_mode :: Ptr () -> IO Bool

setSingleLine :: Text -> Bool -> IO ()
setSingleLine t b = withPtr t $ \p -> clutter_text_set_single_line_mode p b

getSingleLine :: Text -> IO Bool
getSingleLine t = withPtr t $ \p -> clutter_text_get_single_line_mode p



foreign import ccall "clutter_text_insert_text"
  clutter_text_insert_text :: Ptr () -> CString -> CInt -> IO ()

insertText :: Text -> String -> Word -> IO ()
insertText t s n = withPtr t $     \tp ->
                   withCString s $ \sp ->
                   clutter_text_insert_text tp sp (fromIntegral n)

appendText :: Text -> String -> IO ()
appendText t s   = withPtr t $     \tp ->
                   withCString s $ \sp ->
                   clutter_text_insert_text tp sp (-1)


foreign import ccall "clutter_text_delete_chars"
  clutter_text_delete_chars :: Ptr () -> CUInt -> IO ()

deleteChars :: Text -> Word -> IO ()
deleteChars t n = withPtr t $ \p -> clutter_text_delete_chars p (fromIntegral n)


foreign import ccall "clutter_text_delete_text"
  clutter_text_delete_text :: Ptr () -> CInt -> CInt -> IO ()

deleteText :: Text -> Word -> Word -> IO ()
deleteText t x y = withPtr t $ \p ->
                   clutter_text_delete_text p (fromIntegral x) (fromIntegral y)


foreign import ccall "clutter_text_delete_selection"
  clutter_text_delete_selection :: Ptr () -> IO Bool

deleteSelection :: Text -> IO Bool
deleteSelection t = withPtr t $ \p -> clutter_text_delete_selection p


foreign import ccall "clutter_text_get_chars"
  clutter_text_get_chars :: Ptr () -> CInt -> CInt -> IO CString

getChars :: Text -> Word -> Word -> IO String
getChars t x y = withPtr t $ \p ->
                 peekCString =<< clutter_text_get_chars p
                                        (fromIntegral x) (fromIntegral y)


foreign import ccall "clutter_text_set_cursor_color"
  clutter_text_set_cursor_color :: Ptr () -> Ptr Color -> IO ()

foreign import ccall "clutter_text_get_cursor_color"
  clutter_text_get_cursor_color :: Ptr () -> Ptr Color -> IO ()

setCursorColor :: Text -> Color -> IO ()
setCursorColor t c = withPtr t $ \tp ->
                     with c    $ \cp ->
                     clutter_text_set_cursor_color tp cp

resetCursorColor :: Text -> IO ()
resetCursorColor t  = withPtr t $ \p -> clutter_text_set_cursor_color p nullPtr

getCursorColor :: Text -> IO Color
getCursorColor t = withPtr t $ \tp ->
                   with defaultColor $ \cp ->
                   clutter_text_get_cursor_color tp cp >> peek cp



foreign import ccall "clutter_text_set_selection_color"
  clutter_text_set_selection_color :: Ptr () -> Ptr Color -> IO ()

foreign import ccall "clutter_text_get_selection_color"
  clutter_text_get_selection_color :: Ptr () -> Ptr Color -> IO ()

setSelectionColor :: Text -> Color -> IO ()
setSelectionColor t c = withPtr t $ \tp ->
                     with c       $ \cp ->
                     clutter_text_set_selection_color tp cp

resetSelectionColor :: Text -> IO ()
resetSelectionColor t  = withPtr t $ \p ->
                         clutter_text_set_selection_color p nullPtr

getSelectionColor :: Text -> IO Color
getSelectionColor t = withPtr t $ \tp ->
                   with defaultColor $ \cp ->
                   clutter_text_get_selection_color tp cp >> peek cp





foreign import ccall "clutter_text_set_cursor_position"
  clutter_text_set_cursor_position :: Ptr () -> CInt -> IO ()

foreign import ccall "clutter_text_get_cursor_position"
  clutter_text_get_cursor_position :: Ptr () -> IO CInt

setCursorPosition :: Text -> Word -> IO ()
setCursorPosition t b = withPtr t $ \p ->
                   clutter_text_set_cursor_position p (fromIntegral b)

getCursorPosition :: Text -> IO Word
getCursorPosition t = withPtr t $ \p ->
                 fromIntegral `fmap`clutter_text_get_cursor_position p



foreign import ccall "clutter_text_set_cursor_size"
  clutter_text_set_cursor_size :: Ptr () -> CInt -> IO ()

foreign import ccall "clutter_text_get_cursor_size"
  clutter_text_get_cursor_size :: Ptr () -> IO CUInt

setCursorSize :: Text -> Word -> IO ()
setCursorSize t b = withPtr t $ \p ->
                   clutter_text_set_cursor_size p (fromIntegral b)

resetCursorSize :: Text -> IO ()
resetCursorSize t = withPtr t $ \p ->
                      clutter_text_set_cursor_size p (-1)

getCursorSize :: Text -> IO Word
getCursorSize t = withPtr t $ \p ->
                 fromIntegral `fmap`clutter_text_get_cursor_size p






foreign import ccall "clutter_text_set_cursor_visible"
  clutter_text_set_cursor_visible :: Ptr () -> Bool -> IO ()

foreign import ccall "clutter_text_get_cursor_visible"
  clutter_text_get_cursor_visible :: Ptr () -> IO Bool

setCursorVisible :: Text -> Bool -> IO ()
setCursorVisible t b = withPtr t $ \p -> clutter_text_set_cursor_visible p b

getCursorVisible :: Text -> IO Bool
getCursorVisible t = withPtr t $ \p -> clutter_text_get_cursor_visible p







--------------------------------------------------------------------------------

onActivate :: Text -> IO () -> IO HandlerId
onActivate t k = withPtr t $ \p -> signalConnect p "activate" $ void k

onTextChanged :: Text -> IO () -> IO HandlerId
onTextChanged t k = withPtr t $ \p -> signalConnect p "text-changed" $ void k




