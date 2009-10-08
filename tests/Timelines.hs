import Clutter
import Cogl

main :: IO ()
main  = application $ do
  stage <- stageGetDefault
  setColor stage (Color 0 0 0 0xff)
  setSize stage 320 200

  rect1 <- newRectangleWithColor (Color 0x33 0x99 0x33 0x90)
  setSize rect1 100 100
  setPosition rect1 50 50
  setBorderWidth rect1 3
  setBorderColor rect1 (Color 0xff 0xff 0xff 0x90)
  stage `addActor` rect1

  rect <- newTextWithColor (Font "") "Click me!" (Color 0xff 0x00 0xff 0x90)
  setPosition rect 100 100
  setReactive rect True
  stage `addActor` rect

  actorShow rect
  actorShow stage

  rect `onButtonPress` \_ -> do
    putStrLn "Clicked!"
    (d,_,_,_) <- getRotation rect yAxis
    setRotation rect yAxis (d + 10) 0 0 0
    return True

  t1 <- newTimeline 3600
  t1 `onNewFrame` \ms -> do
    setPosition rect (sin (fromIntegral ms * pi / 1800) * 100 + 100) 100

  t2 <- newTimeline 3600
  t2 `onNewFrame` \ms -> do
    setPosition rect 100 (sin (fromIntegral ms * pi / 1800) * 100 + 100)

  s <- newScore
  startWith s t1
  sequenceTimelines s t1 t2
  setLoop s True

  start s
