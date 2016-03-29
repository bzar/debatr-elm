import Model
import View
import Control
import StartApp.Simple as StartApp

main =
  StartApp.start { model = Model.defaultModel, view = View.view, update = Control.update }

