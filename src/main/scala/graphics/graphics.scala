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
import scalafx.scene.paint.Color._
import scalafx.scene.image.Image
import scalafx.scene.image._
import scalafx.beans.property._
import scalafx.geometry.Rectangle2D

import scala.math.sqrt

import position._
import game._
import map._
import messageHandler._
import animation._

import client._

import upickle.default._
import json._
object GraphicEntity
{
  implicit val rw: ReadWriter[GraphicEntity] =
      readwriter[ujson.Value].bimap[GraphicEntity](
        e => JsonTools.write(e, List("animation", "pos", "animationDuration", "frameLength", "_freeze")),
        json => read[GraphicEntity](json)
    )
}

class GraphicEntity(val animation:Animation, val pos:Point, var dest:GraphicsContext) extends Serializable
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
    val viewport = animation.frames(currentFrame).getViewport()
    val frame = animation.frames(currentFrame).getImage()

    // We set the offset based on the offset given as a parameter and the player position
    val off = new Point(Game.player.pos)
    
    val x = w.value/2 + GameWindow.tileSize * (sqrt(3) * (pos.x-off.x)  +  sqrt(3)/2 * (pos.y - off.y))
    val y = h.value/2 + GameWindow.tileSize * (                                3.0/2 * (pos.y - off.y))
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

  def eventHandler(kc:KeyCode)={Game.eventHandler(kc)}
  scene.onKeyPressed = (e:KeyEvent) => eventHandler(e.getCode)

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
    Map.map = Client.getMap()
    println("Client init done")
  }
}
