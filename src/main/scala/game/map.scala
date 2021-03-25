package map

import item._
import graphics._
import messageHandler._
import entity._
import position._
import game._
import scalafx.scene.paint.Color._

class Tile(val coord:Point)
{
    var item:Option[Item] = None
    var entity:Option[SentientEntity] = None
    var walkable:Boolean = true   // can be useful to implement doors and walls
    var seeThrough:Boolean = true // for exemple walls are not see through
    var selected:Boolean = false  // if a tile is selected dipslay info on what it contains
    var seen:Boolean = false      // if a tile has been seen the texture changes accordingly
    var highlight:Boolean = false // indicates if the tile should be "highlighted"
    var highlightAttack:Boolean = false // to show the zone that will take dammage
  
    val highlightTexture:GraphicEntity = new GraphicEntity(AnimationLoader.load("highlightTexture.png", 1), coord, GameWindow.contextGame)
    var backTexture:GraphicEntity      = new GraphicEntity(AnimationLoader.load("texture.png", 1), coord, GameWindow.contextGame)
    var frontTexture:Option[GraphicEntity] = None
    val seenTexture:GraphicEntity      = new GraphicEntity(AnimationLoader.load("seenTexture.png", 1), coord, GameWindow.contextGame)
    val unseenTexture:GraphicEntity    = new GraphicEntity(AnimationLoader.load("unseenTexture.png", 1), coord, GameWindow.contextGame)
    
    val infoDest = GameWindow.contextMenu

    def show() = 
    {
        if(isVisible())
        {
            backTexture.show()
            frontTexture match
            {
              case None => ()
              case Some(g) => g.show()
            }
            if(selected)
            {
                displayInfo()
            }
            if(isHighlighted())
            {
                highlightTexture.show()
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
            frontTexture match
            {
              case None => ()
              case Some(g) => g.show()
            }
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

      MessageHandler.cellInfo.addMessage("Tile at (%d, %d) : %s, %s".format(coord.x, coord.y, se, si))
    }

    def isVisible():Boolean = 
    {
        val d = coord.distance(Game.player.pos)
        val b:Boolean = d <= Game.player.getSeeRange() && Map.inSight(Game.player.pos, coord)
        if(!seen && b)
        {
            seen = true
        }
        return b 
    }
    def isHighlighted():Boolean =
    {
      return highlight || highlightAttack
    }
}

class Wall(coord:Point) extends Tile(coord)
{
  frontTexture = Some(new GraphicEntity(AnimationLoader.load("wall.png", 1), coord, GameWindow.contextGame))
  walkable = false
  seeThrough = false
}

class Door(coord:Point) extends Tile(coord)
{
  frontTexture = Some(new GraphicEntity(AnimationLoader.load("door.png", 1), coord, GameWindow.contextGame))
  walkable = false
  seeThrough = false
}

object Map
{
    var tileArray:Array[Array[Tile]] = Array.ofDim[Array[Tile]](0)

    /** Create the tiles of the map to the size of the given radius
     *  @param radius the radius of the map to be created
     */
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
                if (i == 10 || j == 10)
                  tileArray(i)(j) = new Wall(new Point(i,j))
                else
                  tileArray(i)(j) = new Tile(new Point(i, j))
            }
        }
        tileArray(10)(5) = new Door(new Point(10,5))
    }

    createMap(10)

    /** Display the entirety of the map on the screen */
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

    /** Set the highlight for the designated zone
     *  @param zone a function taking a point and returning a boolean iff the case at the given pos should be highlighted
     *  @param attackHighlight indicates if the highlight to be set is the one indicating tiles that can be attacked (default: false)
     *  @param erase a boolean indicating if the previously set highlight and attack highligh should be erased
     *  @param highlightPlayer should be set to true to highlight the player pos if in zone
     */
    def setHighlight(zone:(Point=>Boolean), attackHighlight:Boolean=false, erase:Boolean=true, highlightPlayer:Boolean=false):Unit =
    {
        var i = 0
        var j = 0
        for (i<-0 until tileArray.size)
        {
            for(j<-0 until tileArray(i).size)
            {
                tileArray(i)(j).highlight = tileArray(i)(j).highlight && !erase// erase previous highlight
                tileArray(i)(j).highlightAttack = tileArray(i)(j).highlightAttack && !erase
              
                if (zone(tileArray(i)(j).coord) && tileArray(i)(j).isVisible() && !tileArray(i)(j).isInstanceOf[Wall] && inSight(Game.player.pos, new Point(i,j)))
                {
                    tileArray(i)(j).highlight = !attackHighlight
                    tileArray(i)(j).highlightAttack = attackHighlight
                }
            }
        }
        Map.fromPoint(Game.player.pos).highlight = Map.fromPoint(Game.player.pos).highlight && highlightPlayer
    }

    def findHighlight():Point =
    {
        var i = 0
        var j = 0

        if (fromPoint(Game.cursor.pos).highlight)
          return Game.cursor.pos

        for (i<-0 until tileArray.size)
        {
            for(j<-0 until tileArray(i).size)
            {
                if (tileArray(i)(j).highlight|| tileArray(i)(j).highlightAttack)
                    return tileArray(i)(j).coord
            }
        }
        return new Point(-1, -1)
    }

    /** Indicates if the destination is visible from the source
     *  @param source the source of the request
     *  @param dest the position of the tile to test
     *  @return a Boolean indicating if the dest is visible from the source
     */
    def inSight(source:Point, dest:Point):Boolean =
    {
      // returns true iff [dest] can be seen from [source]
      // ie, there is only seeThrough tiles between them
      var x = source.x.toDouble
      var y = source.y.toDouble
      val d = source.distance(dest)
      var dx = (dest.x - source.x) / d.toDouble
      var dy = (dest.y - source.y) / d.toDouble
      var i = 0
      var result = true
      for(i <- 0 to d-1)
      {
        result = result && tileArray(x.toInt)(y.toInt).seeThrough
        x += dx
        y += dy

      }
      return result
    }

    /** Return a tile at a given position
     *  @param p the point indicating the position of the tile on the map
     *  @return the tile at position p
     *  The function does not check if the coordinates are inboud
     */
    def fromPoint(p:Point):Tile =
    {
      return tileArray(p.x)(p.y)
    }

    def isInbound(p:Point):Boolean =
    {
        p.x >= 0 && p.y >= 0 && p.x < Map.tileArray.size && p.y < Map.tileArray(p.x).size
    }

}

object Zones
{
    def classic(weapon:Weapon, dir:Int, start:Point, dest:Point):Boolean =
    {
        val d = start.distance(dest)
        (weapon.innerRange <= d) && (d <= weapon.range) && Map.inSight(start, dest)
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

    def singleTile(weapon:Weapon, dir:Int, start:Point, dest:Point):Boolean =
    {
      dest.equals(Game.cursor.pos) || dest.equals(Game.player.pos) // 2nd case is here to allow enemy to use single tile weapon
    }

}
