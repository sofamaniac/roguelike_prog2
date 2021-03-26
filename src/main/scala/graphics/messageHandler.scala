package messageHandler

import scalafx.Includes._
import graphics._
import game._
import scalafx.scene.paint.Color._
import scalafx.beans.property._

object MessageHandler
{
  var help = new MessageZone()
  var inventory = new MessageZone()
  var cellInfo = new MessageZone()
  cellInfo.maxMessages()=2
  var genInfo = new MessageZone()

  var textSize = IntegerProperty(20)

  help.addDefaults("Press F1 to display help")
  help.addDefaults("Use Left/Right arrow to change orientation and Up/Down to move")
  help.addDefaults("Enter to end turn")
  help.addDefaults("Use 'A' to go in attack mode, 'I' to go in information mode")
  help.addDefaults("Use Space to select the current tile")
  help.addDefaults("Use 'Esc' to go back in movement mode")
  help.addDefaults("To use item, press 'E' to go into inventory mode,")
  help.addDefaults("select item using arrow keys, and press 'Space'")
  help.addDefaults("Press 'F' to drop item")
  help.addDefaults("Use 'G' to pick up item")
  
  inventory.addDefaults("Inventory: ")
  inventory.maxMessages() = 11 // 10 items + header

  cellInfo.addDefaults("Tile Info: ")

  clear()

  val one = IntegerProperty(1)

  inventory.yOffset <== when (help.maxMessages > 0) choose (help.yOffset + textSize * (help.maxMessages + one)) otherwise (help.yOffset + textSize * (help.nbMessages + one))
  cellInfo.yOffset <== inventory.yOffset + textSize * (inventory.maxMessages + one)
  genInfo.yOffset <== cellInfo.yOffset + textSize * (cellInfo.maxMessages + one)

  def show():Unit =
  {
    help.show()
    inventory.show()
    cellInfo.show()
    genInfo.show()
  }

  def clear():Unit =
  {
    help.clear()
    // inventory.clear()  // inventory must be cleared manually by the user
    cellInfo.clear()
    genInfo.clear()
  }

  def setHelp():Unit = 
  {
    if (help.maxMessages() == 1) // help is not visible
    {
      help.maxMessages() = 9
    }
    else
    {
      help.maxMessages() = 1
    }
  }
}

class MessageZone(){

  var messages = Vector[String]()
  var defaults = Vector[String]()
  var maxMessages = IntegerProperty(-1) // Maximum number of messages to display, -1 for no limits
  var nbMessages = IntegerProperty(defaults.length)
  var yOffset = IntegerProperty(0)
  var textSize = 20

  // Maybe draw a box aroud each text zone
  // and why not add mechanism that automagically scroll through if there are too many messages

  def addMessage(s:String):Unit=
  {
    messages = messages :+ s
    nbMessages() += 1
  }

  def clear():Unit =
  {
    messages = defaults // TODO: clone vector properly
    nbMessages = IntegerProperty(defaults.length)
  }

  def clearDefaults():Unit =
  {
    defaults = Vector[String]()
  }

  def addDefaults(s:String):Unit=
  {
    defaults = defaults :+ s
  }

  def show():Unit=
  {
    // TODO: fill the occupied space with grey before show
    var y = yOffset + textSize
    GameWindow.contextMenu.setFill(Black)
    var c = 0
    for(i <- messages)
    {
      if (maxMessages() < 0 || c < maxMessages()){
        GameWindow.contextMenu.fillText(i, 0, y.doubleValue())
        y += textSize
      }
      c += 1
    }
    GameWindow.contextMenu.setFill(Grey)
  }
}
