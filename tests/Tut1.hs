import qualified Clutter

main = do 
  Clutter.initClutter
  stage <- Clutter.getDefault
  Clutter.setSize stage 200 200
  Clutter.show stage
  Clutter.main
