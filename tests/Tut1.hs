import Clutter
import System.Exit
import System.IO

main = application $
  do stage <- stageGetDefault
     actorSetSize stage 200 200
     stageSetColor stage (Clutter.Color 255 0 0 0)
     actorShow stage

     fixIO $ \hid -> stage `onButtonPress` \ev ->
                      do x <- btnX ev
                         y <- btnY ev
                         putStrLn ("Hello! " ++ show (x,y))
                         signalDisconnect hid
                         stage `onButtonPress` \ev ->
                            do x <- btnX ev
                               y <- btnY ev
                               putStrLn ("Goodbye!" ++ show (x,y))
                               return True
                         return True

     return ()
