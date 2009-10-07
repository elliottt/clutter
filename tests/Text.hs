import Clutter

text s txt =
  do r <- newTextWithColor (Font "Ariel") txt (Color 0xff 0 0 0xff)
     addActor s r

main = application $
  do stage <- stageGetDefault
     setSize stage 640 480
     setColor stage (Clutter.Color 0 0 0 0xff)
     text stage "Hello"

     t <- newText (Font "Ariel") "Word"
     setColor t (Color 0 0xff 0 0xff)
     setPosition t 50 50
     addActor stage t

     actorShow stage
     return ()
