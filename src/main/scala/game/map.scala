package map

import item._
import graphics._
import entity._
import position._

class Tile(val coord:Point)
{
    var item:Option[Item] = None
    var entity:Option[SentientEntity] = None

    val texture:GraphicEntity = new GraphicEntity(AnimationLoader.load("invisigrid_white.png", 1), new Point(coord.x, coord.y), GameWindow.contextGame)
    def show() = 
    {
        texture.show()
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