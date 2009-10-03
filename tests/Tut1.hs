import Clutter
import System.Exit
import System.IO

main = application $
  do stage <- stageGetDefault
     actorSetSize stage 200 200
     stageSetColor stage (Clutter.Color 255 0 0 0)
     actorShow stage

     fixIO $ \hid ->stage `onButtonPress` \ev ->
                      do putStrLn "Hello!"
                         signalDisconnect hid
                         stage `onButtonPress` \ev ->
                            do x  <- btnX ev
                               y  <- btnY ev
                               cs <- btnClicks ev
                               m  <- btnModifiers ev
                               btn <- btnButton ev
                               putStrLn ("Goodbye!"
                                                ++ " button " ++ show btn
                                                ++ " clicks " ++ show cs
                                                ++ " @ " ++ show (x,y)
                                                ++ " mods " ++ show m)
                               return True
                         return True


     stage `onButtonRelease` \ev ->
       do m <- btnModifiers ev
          print m
          return True
     return ()
