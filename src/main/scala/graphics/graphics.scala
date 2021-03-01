package graphics

import scalafx.animation.AnimationTimer
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.stage.Stage
import scalafx.event.EventHandler
import scalafx.scene.Scene
import scalafx.scene.input.KeyCode
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout._
import scalafx.scene.canvas.Canvas
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.text.Font
import scalafx.scene.text.FontWeight
import scalafx.scene.text.FontPosture
import scalafx.scene.paint.Color._
import scalafx.scene.image.Image
import scalafx.scene.image._
import scalafx.beans.property._
import scalafx.geometry.Rectangle2D

import scala.math.sqrt

import position._
import game._
import map._

class GraphicEntity(val animation:Array[ImageView], val pos:Point, var dest:GraphicsContext)
{

  var animationDuration : Int = 60 // duration in frame of the entire cycle
  var currentFrame = 0
  var frameLength = animationDuration / animation.size
  var frameCounter = 0 // keep count of how many frames the current frame has been displayed
  var _freeze : Boolean = (animation.size == 1)
  val w = GameWindow.canvasGame.width
  val h = GameWindow.canvasGame.height

  def show() : Unit =
  {
    val viewport = animation(currentFrame).getViewport()
    val frame = animation(currentFrame).getImage()
    val xOff = Game.player.pos.x
    val yOff = Game.player.pos.y
    val x = w.value/2 + GameWindow.tileSize * (sqrt(3) * (pos.x-xOff)  +  sqrt(3)/2 * (pos.y - yOff))
    val y = h.value/2 + GameWindow.tileSize * (                               3.0/2 * (pos.y - yOff))
    dest.drawImage(frame, viewport.getMinX(), viewport.getMinY(), viewport.getWidth(), viewport.getHeight(), x, y, viewport.getWidth(), viewport.getHeight())
    updateFrame()
  }
  def updateFrame() : Unit = 
  {
    if(!_freeze)
    {
      frameCounter += 1
      if (frameCounter > frameLength)
      {
        currentFrame = (currentFrame + 1) % animation.size
        frameCounter = 0
      } 
    }
  }
  def setAnimationDuration(t: Int) : Unit = 
  {
    animationDuration = t
    frameLength = t / animation.size
  }
  def freeze():Unit=
  {
    _freeze = !_freeze
  }
}

object AnimationLoader 
{ 
  val ressource_folder = "file:src/main/ressources/"
  def load(s:String, nbFrame: Int, sizeX:Int = -1, sizeY:Int = -1, marginX:Int = 0, marginY:Int = 0):Array[ImageView]=
  {
    var _sizeX = sizeX
    var _sizeY = sizeY
    val image = new Image(ressource_folder + s)
    if(sizeX < 0){
      _sizeX = image.getWidth().toInt / nbFrame
    }
    if(sizeY < 0){
      _sizeY = (image.getHeight()).toInt
    }
    val animation = new Array[ImageView](nbFrame) // Array of length [nbFrame]
    var x = 0
    var i = 0
    for (i <- 0 until nbFrame)
    {
      val frame = new ImageView
      frame.image = image
      frame.setViewport(new Rectangle2D(x, 0, _sizeX, _sizeY)) // we use viewport to define sub pictures
      animation(i) = frame
      x = x + _sizeX + marginX
    }
    return animation
  }
}

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

object GameWindow
{

  val window = new JFXApp.PrimaryStage
  val width = 1920
  val height = 1080
  window.height = height
  window.width  = width
  val tileSize = 32
  
  val canvasGame = new Canvas
  {
    width  <== DoubleProperty(0.70)*window.width // we link the canvas' size to the window's size using properties
    height <== window.height
  }
  val canvasMenu = new Canvas
  {
    width  <== window.width - canvasGame.width
    height <== window.height
  }

  val contextGame = canvasGame.graphicsContext2D
  val contextMenu = canvasMenu.graphicsContext2D
  contextGame.setFill(Black)
  contextMenu.setFill(Grey)
  
  val grid = new GridPane
  grid.add(canvasGame, 0, 0)
  grid.add(canvasMenu, 1, 0)

  val scene = new Scene { root = grid }

  window.setScene(scene)

  def gameHandler(kc:KeyCode)={Game.eventHandler(kc)}
  var currentHandler = "Game"


  def eventHandle(kc:KeyCode):Unit =
  {
    // for now the menu does not handle any keyboard event
    gameHandler(kc)
  }

  scene.onKeyPressed = (e:KeyEvent) => eventHandle(e.getCode)

  val loop = AnimationTimer
  {
    t=>
      clearScreen()
      Map.show()
      Game.cursor.show()
      MessageHandler.show()
  }

  def clearScreen():Unit =
  {
    contextGame.fillRect(0, 0, canvasGame.getWidth, canvasGame.getHeight)
    contextMenu.fillRect(0, 0, canvasMenu.getWidth, canvasMenu.getHeight)
  }


  def start():Unit =
  {
    Game.initialization()
    loop.start()
  }
}
