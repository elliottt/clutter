import Clutter
import Clutter.Behavior
import Clutter.Timeline
import Clutter.Path
import Clutter.Behavior.Path
import Clutter.Alpha
import Clutter.AnimationMode

rectangle s =
  do r <- newRectangleWithColor (Color 0xff 0 0 0x99)
     setSize r 100 100
     setPosition r 20 20
     setBorderWidth r 2
     setBorderColor r (Color 0xff 0xff 0xff 0x99)
     addActor s r
     return r

path r =
  do t <- newTimeline 1000
     a <- newAlpha t mLinear
     p <- newPath
     move p (20,20)
     relLine p (100,0)
     relLine p (0,100)
     close p
     b <- newPathBehavior a p
     apply b r
     return t
      


main = application $
  do stage <- stageGetDefault
     setSize stage 640 480
     setColor stage (Clutter.Color 0 0 0 0xff)
     r <- rectangle stage
     t <- path r

     onButtonPress stage $ \_ -> start t >> return True
     actorShow stage
     return ()
