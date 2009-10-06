
import Clutter
import Cogl

main :: IO ()
main  = application $ do
  stage <- stageGetDefault
  stageSetColor stage (Color 0 0 0 0xff)
  actorSetSize stage 320 200

  rect <- newRectangleWithColor (Color 0xff 0x90 0x90 0xff)
  actorSetSize rect 100 100
  actorSetPosition rect 100 100
  actorSetReactive rect True
  stage `addActor` rect

  actorShow rect
  actorShow stage

  t <- newTimeline 3600
  setLoop t True

  t `onNewFrame` \ms -> do
    actorSetPosition rect (sin (fromIntegral ms * pi / 1800) * 100 + 100) 100

  rect `onButtonPress` \_ -> do
    putStrLn "Clicked!"
    (d,_,_,_) <- getRotation rect yAxis
    setRotation rect yAxis (d + 10) 0 0 0
    return True

  startTimeline t
