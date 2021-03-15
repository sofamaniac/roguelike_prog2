package messageHandler

import graphics._
import game._
import scalafx.scene.paint.Color._

object MessageHandler
{
  var messages = Vector[String]()
  var inventory = Vector[String]()
  var cellMessage = ""
  val textSize = 20
  def addInfo(s:String):Unit =
  {
    messages = messages :+ s
  }
  def cellInfo(s:String):Unit =
  {
    cellMessage = s
  }

  def show():Unit =
  {
    var displayInfoY = textSize
    GameWindow.contextMenu.setFill(Black)

    for(i <- messages)
    {
      GameWindow.contextMenu.fillText(i, 0, displayInfoY)
      displayInfoY += textSize
    }
    GameWindow.contextMenu.fillText(cellMessage, 0, displayInfoY)
    displayInfoY += textSize

    for(i<- inventory)
    {
      GameWindow.contextMenu.fillText(i, 0, displayInfoY)
      displayInfoY += textSize
    }
    GameWindow.contextMenu.setFill(Grey)
  }

  def clear():Unit =
  {
    messages = Vector[String]()

    addInfo("Use Left/Right arrow to change orientation and Up/Down to move")
    addInfo("Use 'A' to go in attack mode, 'I' to go in information mode")
    addInfo("Use Space to select the current tile")
    addInfo("Use 'Esc' to go back in movement mode")
    addInfo("To use item, press 'E' to go into inventory mode,")
    addInfo("select item using arrow keys, and press 'Space'")
    addInfo("Press 'F' to drop item")
    addInfo("Use 'G' to pick up item")
    addInfo("Player: %d/%d HP; %d/%d(+%d) AP".format(Game.player.curHP, Game.player.maxHP, Game.player.curAP, Game.player.baseAP, Game.player.modifAP))
  }

  def clearInventory():Unit =
  {
    inventory = Vector[String]()
    addInventory("Current Weapon : %s".format(Game.player.weapon.getInfo()))
    addInventory("Inventory:")
  }

  def addInventory(s:String):Unit = 
  {
    inventory = inventory :+ s
  }
}

