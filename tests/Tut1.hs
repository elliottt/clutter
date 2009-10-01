import Clutter



main = application $
  do stage <- stageGetDefault
     actorSetSize stage 200 200
     stageSetColor stage (Clutter.Color 255 0 0 0)
     actorShow stage
     signalConnect stage "button-press-event" $ \ _ _ -> do
       putStrLn "Hello!"
       return True
     return ()
