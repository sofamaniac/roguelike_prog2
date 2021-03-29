package animation

import scalafx.scene.image.Image
import scalafx.scene.image._
import scalafx.geometry.Rectangle2D
import upickle.default._
import json._

object Animation
{ 
  type Animation = Array[ImageView]
  val ressource_folder = "file:src/main/ressources/graphics/"
  def load(s:String, nbFrame: Int, sizeX:Int = -1, sizeY:Int = -1, marginX:Int = 0, marginY:Int = 0):Animation=
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
    val animation = new Animation(nbFrame) // Array of length [nbFrame]
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

  def loadJson(json:ujson.Value):Animation = {
    return load("goblin.png", 11, sizeY=58)
  }
}