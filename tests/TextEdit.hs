import Clutter
import System.IO(hFlush,stdout)

main = application $
  do stage <- stageGetDefault
     setSize stage 640 480
     setColor stage (Clutter.Color 0 0 0 0xff)

     t <- newText (Font "huh") "Word"
     setColor t (Color 0 0xff 0 0xff)
     setPosition t 50 50
     addActor stage t

     t `onTextChanged` (putChar '.' >> hFlush stdout)
     t `onActivate`
        do txt <- getText t
           sel <- getSelection t
           putStrLn txt
           putStrLn sel
           setNotPassword t

     setPassword t '*'
     setSelectionColor t (Color 0xff 0 0 0x66)
     setCursorColor t (Color 0 0x99 0x99 0x99)
     setCursorSize t 10
     setActivatable t True
     setSelectable t True
     setKeyFocus stage t
     setEditable t True

     actorShow stage
     return ()
