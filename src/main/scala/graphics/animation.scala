package animation

import scalafx.scene.image.Image
import scalafx.scene.image._
import scalafx.geometry.Rectangle2D
import upickle.default._
import json._

object Animation
{ 
  type AnimationType = Array[ImageView]
  
  implicit val rw: ReadWriter[Animation] =
    readwriter[ujson.Value].bimap[Animation](
      e => writeJson(e),
      json => loadJson(json)
    )

  val ressource_folder = "file:src/main/ressources/graphics/"
  def load(s:String, nbFrame: Int, sizeX:Int = -1, sizeY:Int = -1, marginX:Int = 0, marginY:Int = 0):AnimationType=
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
    val animation = new AnimationType(nbFrame) // Array of length [nbFrame]
    var x = 0
    var i = 0
    for (i <- 0 until nbFrame)
    {
      val frame = new ImageView
      frame.image = image
      frame.setViewport(new Rectangle2D(x, marginY, _sizeX, _sizeY)) // we use viewport to define sub pictures
      animation(i) = frame
      x = x + _sizeX + marginX
    }
    return animation
  }

  val defPath = "goblin.png"
  val defNbFrame = 1
  val defSizeX = -1
  val defSizeY = -1
  val defMargX = 0
  val defMargY = 0

  def loadJson(json:ujson.Value):Animation = {
    val path    = JsonTools.load(json, "file", defPath)
    val nbFrame = JsonTools.load(json, "nbFrame", defNbFrame)
    val sizeX   = JsonTools.load(json, "sizeX", defSizeX)
    val sizeY   = JsonTools.load(json, "sizeY", defSizeY)
    val margX   = JsonTools.load(json, "marginX", defMargX)
    val margY   = JsonTools.load(json, "marginY", defMargY)
    return new Animation(path, nbFrame, sizeX, sizeY, margX, margY)
  }

  def loadDefault():Animation = {
    return new Animation(defPath, defNbFrame, defSizeX, defSizeY, defMargX, defMargY)
  }

  def writeJson(a:Animation):ujson.Value =
  {
    return JsonTools.write(a, List("path", "nbFrame", "sizeX", "sizeY", "margX", "margY") )
  }
}

case class Animation(val path:String, val nbFrame:Int, val sizeX:Int = -1, val sizeY:Int = -1, val margX:Int = 0, val margY:Int = 0)
{
  val frames = Animation.load(path, nbFrame, sizeX, sizeY, margX, margY)
  val size = frames.size
}
