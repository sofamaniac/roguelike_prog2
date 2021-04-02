package map

import item._
import graphics._
import messageHandler._
import animation._
import entity._
import position._
import game._
import scalafx.scene.paint.Color._
import scala.collection.mutable.{Map => MapObject}

class Tile(val coord:Point)
{
    var item:Option[Item] = None
    var entity:Option[SentientEntity] = None

    var walkable:Boolean = true   // can be useful to implement doors and walls
    var seeThrough:Boolean = true // for exemple walls are not see through
    var flyable:Boolean = true    // can a flying entity be here
    
    var selected:Boolean = false  // if a tile is selected dipslay info on what it contains
    var seen:Boolean = false      // if a tile has been seen the texture changes accordingly
    var highlight:Boolean = false // indicates if the tile should be "highlighted"
    var highlightAttack:Boolean = false // to show the zone that will take dammage
  
    val highlightAttackTexture:GraphicEntity = new GraphicEntity(Animation.load("highlightAttackTexture.png", 1), coord, GameWindow.contextGame)
    val highlightTexture:GraphicEntity = new GraphicEntity(Animation.load("highlightTexture.png", 1), coord, GameWindow.contextGame)
    var backTexture:GraphicEntity      = new GraphicEntity(Animation.load("texture.png", 1), coord, GameWindow.contextGame)
    var frontTexture:Option[GraphicEntity] = None
    val seenTexture:GraphicEntity      = new GraphicEntity(Animation.load("seenTexture.png", 1), coord, GameWindow.contextGame)
    val unseenTexture:GraphicEntity    = new GraphicEntity(Animation.load("unseenTexture.png", 1), coord, GameWindow.contextGame)
    
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
            if(isHighlighted())
            {
                highlightTexture.show()
            }
            if(highlightAttack)
            {
                highlightAttackTexture.show()
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

      MessageHandler.setCellMessage("Tile at (%d, %d) : %s, %s".format(coord.x, coord.y, se, si))
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
    def select(b:Boolean):Unit =
    {
      selected = b
      if(b)
        displayInfo()
    }
}

class Wall(coord:Point) extends Tile(coord)
{
  frontTexture = Some(new GraphicEntity(Animation.load("wall.png", 1), coord, GameWindow.contextGame))
  walkable = false
  seeThrough = false
  flyable = false
}

class Door(coord:Point) extends Tile(coord)
{
  frontTexture = Some(new GraphicEntity(Animation.load("door.png", 1), coord, GameWindow.contextGame))
  walkable = false
  seeThrough = false
  flyable = false

  def open():Unit = { 
    frontTexture = None
    walkable = true
    seeThrough = true
    flyable = true
  }
}

object Map
{
    var tileMap = MapObject[(Int, Int), Tile]()

    /** Create the tiles of the map to the size of the given radius
     *  @param radius the radius of the map to be created
     */
    def createMap(radius:Int)
    {
        var i = 0
        var j = 0
        for (i <- 0 until 2*radius +1)
        {
            for (j <- 0 until 2*radius +1)
            {
                val b = (i,j)
                if (i == 10 || j == 10)
                  tileMap += (b -> new Wall(new Point(i,j)))
                else
                  tileMap += (b -> new Tile(new Point(i,j)))
            }
        }
        tileMap((0,0)) = new Tile(new Point(0,0))
        tileMap((10,5)) = new Door(new Point(10,5))
    }

    createMap(10)

    /** Display the entirety of the map on the screen */
    def show() = 
    {
        tileMap.foreach
        {
            case(key, value) => value.show()
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
        tileMap.foreach
        {
            case(key, value) =>
                value.highlight = value.highlight && !erase     // erase previous highlight
                value.highlightAttack = value.highlightAttack && !erase
              
                if (zone(value.coord) && value.isVisible() && !value.isInstanceOf[Wall] && inSight(Game.player.pos, new Point(key._1,key._2)))
                {
                    value.highlight = !attackHighlight
                    value.highlightAttack = attackHighlight
                }
        }
        Map.fromPoint(Game.player.pos).highlight = Map.fromPoint(Game.player.pos).highlight && highlightPlayer
    }

    def findHighlight():Point =
    {
        if (fromPoint(Game.cursor.pos).highlight)
          return Game.cursor.pos

        tileMap.foreach
        {
            case(key, value) =>
                if (value.isHighlighted())
                    return value.coord
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
        result = result && tileMap((x.toInt,y.toInt)).seeThrough
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
        return tileMap((p.x,p.y))
    }

    def isInbound(p:Point):Boolean =
    {
        tileMap contains (p.x,p.y)
    }

}
