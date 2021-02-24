package map

import item._
import graphics._
import entity._
import position._

import scala.math.{max, min}

class Tile(val coord:Point)
{
    var isWalkable:Boolean = true
    
    var item:Option[Item] = None
    var entity:Option[SentientEntity] = None

    val texture:GraphicEntity = new GraphicEntity(AnimationLoader.load("tileDirt_tile.png", 1), new Point(coord.x, coord.y), GameWindow.contextGame)
    
    def show() = 
    {
        texture.show()
        /*
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
        */
    }
}

object Map
{
    var tileArray:Array[Array[Tile]] = Array.ofDim[Array[Tile]](0)

    def addHexagon(middle:Point, radius:Int)
    {
        tileArray = Array.ofDim[Array[Tile]](2*radius+1)    // initialisation here (temporary) : TODO : find a way to get the max size before generating the map
        var i = 0
        for (i <- 0 until 2*radius +1)
        {
            tileArray(i) = Array.ofDim[Tile](2*radius +1) // TODO: to change to the correct size
        }

        for (a <- -radius until radius+1)
        {
            var r1:Int = max(-radius, -a - radius)
            var r2:Int = min(radius, -a + radius)
            for (r <- r1 until r2+1)
            {
                tileArray(a)(r) = new Tile(new Point(a, r))
            }
        }
    }

    def createMap(radius:Int)
    {
        addHexagon(new Point(0,0), radius)
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
