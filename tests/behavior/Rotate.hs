import Clutter
import Clutter.Behavior.Rotate
import Clutter.Behavior
import Clutter.Alpha
import Clutter.AnimationMode
import Data.IORef

rectangle s =
  do r <- newRectangleWithColor (Color 0xff 0 0 0x99)
     setSize r 100 100
     setPosition r 200 200
     setBorderWidth r 2
     setBorderColor r (Color 0xff 0xff 0xff 0x99)
     addActor s r
     return r

rotate r =
  do t <- newTimeline 1000
     a <- newAlpha t mLinear
     rb <- newRotateBehavior a yAxis clockwise 0 360
     apply rb r
     return t



main = application $
  do stage <- stageGetDefault
     setSize stage 640 480
     setColor stage (Clutter.Color 0 0 0 0xff)

     r <- rectangle stage
     t <- rotate r

     onButtonPress stage $ \_ -> start t >> return True
     actorShow stage
     return ()
