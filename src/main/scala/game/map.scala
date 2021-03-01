package map

import item._
import graphics._
import entity._
import position._
import game._
import scalafx.scene.paint.Color._

class Tile(val coord:Point)
{
    var item:Option[Item] = None
    var entity:Option[SentientEntity] = None
    var walkable:Boolean = true   // can be useful to implement doors
    var seeThrough:Boolean = true // for exemple walls are not see through
    var selected:Boolean = false  // if a tile is selected dipslay info on what it contains
    var seen:Boolean = false      // if a tile has been seen the texture changes accordingly
    var highlight:Boolean = false // indicates if the tile should be "highlighted"
  
    val highlightTexture:GraphicEntity = new GraphicEntity(AnimationLoader.load("highlightTexture.png", 1), coord, GameWindow.contextGame)
    val texture:GraphicEntity = new GraphicEntity(AnimationLoader.load("texture.png", 1), coord, GameWindow.contextGame)
    val seenTexture:GraphicEntity = new GraphicEntity(AnimationLoader.load("seenTexture.png", 1), coord, GameWindow.contextGame)
    val unseenTexture:GraphicEntity = new GraphicEntity(AnimationLoader.load("unseenTexture.png", 1), coord, GameWindow.contextGame)
    
    val infoDest = GameWindow.contextMenu

    def show() = 
    {
        if(isVisible())
        {
            if(selected)
            {
                displayInfo()
            }

            if(highlight)
            {
                highlightTexture.show()
            }
            else
            {
                texture.show()
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
        else if(seen)
        {
            seenTexture.show()
        }
        else
        {
            unseenTexture.show()
        }
    }

    def displayInfo() =
    {
      var se = ""
      entity match
      {
        case None    => se = "No entity here."
        case Some(e) => se = e.getInfo()
      }
      var si = ""
      item match
      {
        case None    => si = "No item here."
        case Some(i) => si = i.getInfo()
      }

      MessageHandler.cellInfo("Tile at (%d, %d) : %s, %s".format(coord.x, coord.y, se, si))
    }

    def isVisible():Boolean = 
    {
        val d = coord.distance(Game.player.pos)
        val b:Boolean = d <= Game.player.getSeeRange()
        if(!seen && b)
        {
            seen = true
        }
        return b // TODO : update such that non see through tiles obscure the view 
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

    createMap(10)

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

    def setHighlight(zone:(Point=>Boolean)):Unit =
    {
        var i = 0
        var j = 0
        for (i<-0 until tileArray.size)
        {
            for(j<-0 until tileArray(i).size)
            {
                tileArray(i)(j).highlight = false // erase previous highlight
              
                if (zone(tileArray(i)(j).coord) && tileArray(i)(j).isVisible())
                {
                    tileArray(i)(j).highlight = true
                }
            }
        }
    }

    def findHighlight():Point =
    {
        var i = 0
        var j = 0
        for (i<-0 until tileArray.size)
        {
            for(j<-0 until tileArray(i).size)
            {
                if (tileArray(i)(j).highlight)
                {
                    return tileArray(i)(j).coord
                }
            }
        }
        return new Point(-1, -1)
    }

  def fromPoint(p:Point):Tile =
  {
    return tileArray(p.x)(p.y)
  }

}

object Zones
{
    def classic(weapon:Weapon, dir:Int, start:Point, dest:Point):Boolean =
    {
        val d = start.distance(dest)
        (weapon.innerRange <= d) && (d <= weapon.outerRange)
    }

    def ray(weapon:Weapon, dir:Int, start:Point, dest:Point):Boolean =
    {
        val dx = dest.x - start.x
        val dy = dest.y - start.y
        val dz = -dx-dy
        dir match
        {
            case 0 => (dy == 0) && (dx >= 0) && (weapon.innerRange <= dx.abs) && (dx.abs <= weapon.range)
            case 1 => (dx == 0) && (dz <= 0) && (weapon.innerRange <= dz.abs) && (dz.abs <= weapon.range)
            case 2 => (dz == 0) && (dy >= 0) && (weapon.innerRange <= dy.abs) && (dy.abs <= weapon.range)
            case 3 => (dy == 0) && (dx <= 0) && (weapon.innerRange <= dx.abs) && (dx.abs <= weapon.range)
            case 4 => (dx == 0) && (dz >= 0) && (weapon.innerRange <= dz.abs) && (dz.abs <= weapon.range)
            case 5 => (dz == 0) && (dy <= 0) && (weapon.innerRange <= dy.abs) && (dy.abs <= weapon.range)
        }
    }

    def cone(weapon:Weapon, dir:Int, start:Point, dest:Point):Boolean =
    {
        val dx = dest.x - start.x
        val dy = dest.y - start.y
        val dz = -dx-dy
        dir match
        {
            case 0 => (dy <= 0) && (dz <= 0) && (weapon.innerRange <= dx.abs) && (dx.abs <= weapon.range)
            case 1 => (dx >= 0) && (dy >= 0) && (weapon.innerRange <= dz.abs) && (dz.abs <= weapon.range)
            case 2 => (dx <= 0) && (dz <= 0) && (weapon.innerRange <= dy.abs) && (dy.abs <= weapon.range)
            case 3 => (dy >= 0) && (dz >= 0) && (weapon.innerRange <= dx.abs) && (dx.abs <= weapon.range)
            case 4 => (dx <= 0) && (dy <= 0) && (weapon.innerRange <= dz.abs) && (dz.abs <= weapon.range)
            case 5 => (dx >= 0) && (dz >= 0) && (weapon.innerRange <= dy.abs) && (dy.abs <= weapon.range)
        }
    }
}