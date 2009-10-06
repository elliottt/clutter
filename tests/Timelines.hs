
import Clutter
import Cogl

main :: IO ()
main  = application $ do
  stage <- stageGetDefault
  stageSetColor stage (Color 0 0 0 0xff)
  actorSetSize stage 320 200

  rect1 <- newRectangleWithColor (Color 0x40 0x30 0xff 0x90)
  actorSetSize rect1 100 100
  actorSetPosition rect1 50 50
  stage `addActor` rect1

  rect <- newRectangleWithColor (Color 0x40 0xff 0x90 0x90)
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
