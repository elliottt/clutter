import Clutter

rectangle x y w h c bw bc =
  do r <- newRectangle
     setPosition r x y
     setSize r w h
     setColor r c
     setBorderWidth r bw
     setBorderColor r bc
     return r

main :: IO ()

main  = application $ do
  stage <- stageGetDefault
  setColor stage (Color 0 0 0 0xff)
  setSize stage 320 200

  rect1 <- rectangle 50 50 100 100
              (Color 0x33 0x99 0x33 0x90) 3 (Color 0xff 0xff 0xff 0x90)

  g <- newGroup
  addActor g =<< rectangle 0 0 100 100 (Color 0x00 0xff 0xff 0x90)
                                       3 (Color 0xff 0xff 0xff 0x90)
  addActor g =<< newTextWithColor (Font "") "Click me!"
                                      (Color 0xff 0x00 0xff 0x90)
  setReactive g True

  addActor stage rect1
  addActor stage g
  actorShow stage

  g `onButtonPress` \_ -> do
    putStrLn "Clicked!"
    (d,_,_,_) <- getRotation g yAxis
    setRotation g yAxis (d + 10) 0 0 0
    return True

  t1 <- newTimeline 3600
  t1 `onNewFrame` \ms -> do
    setPosition g (sin (fromIntegral ms * pi / 1800) * 100 + 100) 100

  t2 <- newTimeline 3600
  t2 `onNewFrame` \ms -> do
    setPosition g 100 (sin (fromIntegral ms * pi / 1800) * 100 + 100)

  s <- newScore
  startWith s t1
  sequenceTimelines s t1 t2
  setLoop s True

  start s

