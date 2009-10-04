import Clutter

rectangle s =
  do r <- newRectangleWithColor (Color 0xff 0 0 0x99)
     actorSetSize r 100 100
     actorSetPosition r 20 20
     setRectangleBorderWidth r 2
     setRectangleBorderColor r (Color 0xff 0xff 0xff 0x99)
     addActor s r

main = application $
  do stage <- stageGetDefault
     actorSetSize stage 640 480
     stageSetColor stage (Clutter.Color 0 0 0 0xff)
     rectangle stage
     actorShow stage

     return ()
