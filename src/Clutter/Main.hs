module Clutter.Main where

foreign import ccall "clutter_main" main :: IO ()
