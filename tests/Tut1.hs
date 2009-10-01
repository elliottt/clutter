import Clutter
import System.Exit
import System.IO

main = application $
  do stage <- stageGetDefault
     actorSetSize stage 200 200
     stageSetColor stage (Clutter.Color 255 0 0 0)
     actorShow stage
     fixIO $ \hid -> signalConnect stage "button-press-event" $ \_ -> do
       putStrLn "Hello!"
       signalDisconnect stage hid
       signalConnect stage "button-press-event" $ \_ -> do
         putStrLn "Goodbye!"
         exitSuccess
       return True
     return ()
