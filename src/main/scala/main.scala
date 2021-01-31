import scala.swing._

class UI extends MainFrame {
  title = "Fenetre"
  preferredSize = new Dimension(800, 600)
  contents = new Label("Bite")
}

object RogueLikeProgram {
  def main(args:Array[String]) {
    val ui = new UI
    ui.visible = true
  }
}

