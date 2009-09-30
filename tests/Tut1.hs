import qualified Clutter

main = Clutter.application $
  do stage <- Clutter.getDefault
     Clutter.setSize stage 200 200
     Clutter.show stage
