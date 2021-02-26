package map

import item._
import graphics._
import entity._
import position._
import game._

class Tile(val coord:Point)
{
    var item:Option[Item] = None
    var entity:Option[SentientEntity] = None
    var walkable:Boolean = true   // can be useful to implement doors
    var seeThrough:Boolean = true // for exemple walls are not see through
    var selected:Boolean = false  // if a tile is selected dipslay info on what it contains
    var highlight:Boolean = false // indicates if the tile should be "highlighted"

    val texture:GraphicEntity = new GraphicEntity(AnimationLoader.load("tileDirt_tile.png", 1), coord, GameWindow.contextGame)
    val hideTexture:GraphicEntity = new GraphicEntity(AnimationLoader.load("hide_texture.png", 1), coord, GameWindow.contextGame)
    val infoDest = GameWindow.contextMenu

    def show() = 
    {
      texture.show()

      if(isVisible())
      {
        if(selected)
        {
          displayInfo()
        }
        item match
        {
          case None => ()
          case Some(i) => i.show()
        }
        entity match
        {
          case None => ()
          case Some(e) => e.show()
        }
      }
      else
      {
        hideTexture.show()
      }
    }

    def displayInfo() =
    {
      infoDest.fillText(s"Tile at ($coord.x, $coord.y) : Nothing to display", 0, 0)
    }

    def isVisible():Boolean = 
    {
      val d = coord.distance(Game.player.pos)
      return d <= Game.player.getSeeRange() // TODO : update such that non see through tiles obscure the view 
      //(just find a straight line to the player and check if every tile on it is see through)
    }
}

object Map
{
  var tileArray:Array[Array[Tile]] = Array.ofDim[Array[Tile]](0)

  def createMap(radius:Int)
  {
    tileArray = Array.ofDim[Array[Tile]](2*radius+1)
    var i = 0
    for (i <- 0 until 2*radius +1)
    {
      tileArray(i) = Array.ofDim[Tile](2*radius +1) // TODO: to change to the correct size
    }
    
    var j = 0
    for (i <- 0 until 2*radius +1)
    {
      for (j <- 0 until 2*radius +1)
      {
        tileArray(i)(j) = new Tile(new Point(i, j))
      }
    }
  }
  createMap(5)

  def show() = 
  {
    var i = 0
    var j = 0
    for (i <- 0 until tileArray.size)
    {
      for (j <- 0 until tileArray(i).size)
      {
        tileArray(i)(j).show()
      }
    }
  }
}
