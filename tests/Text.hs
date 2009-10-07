import Clutter

text s txt =
  do r <- newTextWithColor (Font "Ariel") txt (Color 0xff 0 0 0xff)
     addActor s r

main = application $
  do stage <- stageGetDefault
     setSize stage 640 480
     setColor stage (Clutter.Color 0 0 0 0xff)
     text stage "Hello"
     actorShow stage

     return ()
