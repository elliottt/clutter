import Clutter

rectangle s =
  do r <- newRectangleWithColor (Color 0xff 0 0 0x99)
     actorSetSize r 100 100
     actorSetPosition r 20 20
     r `setName` "rectangle"
     setRectangleBorderWidth r 2
     setRectangleBorderColor r (Color 0xff 0xff 0xff 0x99)
     addActor s r

main = application $
  do stage <- stageGetDefault
     actorSetSize stage 640 480
     stageSetColor stage (Clutter.Color 0 0 0 0xff)
     rectangle stage
     actorShow stage
     mb <- findByName stage "rectangle"
     case mb of
        Nothing -> putStrLn "Not found!"
        Just a -> do putStrLn "Found!"
                     mb1 <- toRectangle a  
                     case mb1 of
                       Just _  -> putStrLn "Cast successful!"
                       Nothing -> putStrLn "Cast failed!"
     return ()
