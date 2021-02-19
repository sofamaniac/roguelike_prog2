import graphics._
import scalafx.application.JFXApp

object Program extends JFXApp
{
    System.setProperty("quantum.multithreaded", "false");
    Game.test()
}
