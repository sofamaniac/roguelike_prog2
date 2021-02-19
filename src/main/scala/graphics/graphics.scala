package graphics

import scalafx.animation.AnimationTimer
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.Group
import scalafx.scene.layout.Pane
import scalafx.scene.canvas.Canvas
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.scene.text.FontWeight
import scalafx.scene.text.FontPosture;
import scalafx.scene.paint.Color._
import scalafx.scene.image.Image
import scalafx.scene.image._

import position._

class GraphicEntity(val animation: Array[ImageView], val pos: Point, var dest: GraphicsContext){

  var animationDuration : Int = 60 // duration in frame of the entire cycle
  var currentFrame = 0
  var frameLength = animationDuration / animation.size
  var frameCounter = 0 // keep count of how many frames the current frame has been displayed
  var _freeze = if (animation.size == 1) true else false

  def show() : Unit =
  {
    dest.drawImage(animation(currentFrame).getImage(), pos.x, pos.y)
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

object AnimationLoader { 
  val ressource_folder = "file:src/main/ressources/"
  def load_animation(s:String):Array[ImageView]=
  {
    return Array(new ImageView(ressource_folder + "dome.png"))
  }
}
