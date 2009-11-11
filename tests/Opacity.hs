import Clutter
import Clutter.Behavior.Opacity
import Clutter.Behavior
import Clutter.Alpha
import Clutter.AnimationMode
import Data.IORef

rectangle s =
  do r <- newRectangleWithColor (Color 0xff 0 0 0x99)
     setSize r 100 100
     setPosition r 20 20
     setBorderWidth r 2
     setBorderColor r (Color 0xff 0xff 0xff 0x99)
     addActor s r
     return r

flipFlop act d x y =
  do t <- newTimeline d
     a <- newAlpha t mLinear
     o1 <- newOpacity a y x
     o2 <- newOpacity a x y
     r <- newIORef (Right (o1,o2))
     apply o1 act
     onCompleted t $
        do v <- readIORef r
           case v of
             Right _    -> return ()
             Left (a,b) ->
               do remove a act
                  apply b act
                  writeIORef r (Right (b,a))

     return $ do x <- readIORef r
                 case x of
                    Left _ -> putStrLn "no" >> return ()
                    Right (a,b) ->
                      do putStrLn "yes"
                         writeIORef r (Left (a,b))
                         start t
       


main = application $
  do stage <- stageGetDefault
     setSize stage 640 480
     setColor stage (Clutter.Color 0 0 0 0xff)



     r <- rectangle stage

     flip <- flipFlop r 500 0 255
     
     onButtonPress stage $ \_ -> flip >> return True
     actorShow stage
     return ()
