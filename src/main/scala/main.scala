import graphics._
import scalafx.application.JFXApp

object Test {
  System.setProperty("quantum.multithreaded", "false");
}
object Program extends JFXApp
{
    GameWindow.start()
}
