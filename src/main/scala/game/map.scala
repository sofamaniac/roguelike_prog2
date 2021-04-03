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


object CommonTextures
{
    // Some textures are used by all tiles, so we load them in memory only once
    val highlightAttack:GraphicEntity = new GraphicEntity(Animation.load("highlightAttackTexture.png", 1), new Point(0,0), GameWindow.contextGame)
    val highlight:GraphicEntity = new GraphicEntity(Animation.load("highlightTexture.png", 1), new Point(0, 0), GameWindow.contextGame)
    val seen:GraphicEntity      = new GraphicEntity(Animation.load("seenTexture.png", 1), new Point(0, 0), GameWindow.contextGame)
    val unseen:GraphicEntity    = new GraphicEntity(Animation.load("unseenTexture.png", 1), new Point(0, 0), GameWindow.contextGame)

    def showTexture(texture:GraphicEntity, pos:Point)
    {
      texture.pos.setPoint(pos)
      texture.show()
    }
}

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

    var textureMap = MapObject[String, Some(GraphicEntity)]()

    var backTexture:GraphicEntity      = new GraphicEntity(Animation.load("texture.png", 1), coord, GameWindow.contextGame)
    var frontTexture:Option[GraphicEntity] = None
    
    val infoDest = GameWindow.contextMenu

    val textures = CommonTextures

    def show(offset) = 
    {
        if(isVisible(offset))
        {
            backTexture.show()
            frontTexture match
            {
              case None => ()
              case Some(g) => g.show(offset)
            }
            if(highlight)
            {
                textures.show(textures.highlight, coord)
            }
            if(highlightAttack)
            {
                textures.show(textures.highlightAttack, coord)
            }

            item match
            {
                case None => ()
                case Some(i) => i.show(offset)
            }
            entity match
            {
                case None => ()
                case Some(e) => e.show(offset)
            }
        }
        else if(seen)
        {
            textures.show(textures.seen, coord)
            frontTexture match
            {
              case None => ()
              case Some(g) => g.show()
            }
        }
        else
        {
            textures.show(textures.unseen, coord)
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

    def isVisible(offset:Point=new Point(0,0)):Boolean = 
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
    def setOffset(off:Point):Unit =
    {
      coord.add(off)
      item match
      {
        case Some(i) => i.pos.add(off)
      }
      entity match
      {
        case Some(e) => e.pos.add(off)
      }
    }
    def placeItem(droppedItem:Item, from:Option[SentientEntity]):Unit=
    {
      // [from] is used to indicates if the item is placed by some entity and should therefore
      // be removed from her inventory

      var placed = false  // We avoid nested match
      item match
      {
        case Some(i) => ()  // We don't overwrite the current item
        case _       => item = Some(droppedItem)
                        placed = true
      }
      if (placed)
      {
        from match
        {
          case Some(e) => e.inventory.remove(droppedItem)
          case _       => ()
        }
      }
}

class Wall(coord:Point) extends Tile(coord)
{
  frontTexture = Some(new GraphicEntity(Animation.load("wall.png", 1), coord, GameWindow.contextGame))
  walkable = false
  seeThrough = false
  flyable = false
}

object Map
{
  var rooms = MapObject[(Int, Int), Tile]


    /** Display the entirety of the map on the screen */
    def show() = 
    {
        tileMap.foreach
        {
            case(key, room) => room.show()
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
        rooms.foreach
        {
          case (key, room) => room.setHighlight(zone, attackHighlight, erase, highlightPlayer)
        }
        Map.fromPoint(Game.player.pos).highlight = Map.fromPoint(Game.player.pos).highlight && highlightPlayer
    }

    def findHighlight():Point =
    {
      // TODO: iterate over each room, which iterates over its tiles
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
      // TODO: to change
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
        var result = false
        rooms.foreach
        {
            case(key, room) => result = result || room.tiles contains (p.x,p.y)
        }
    }
}

class Door(coord:Point, val room:Room) extends Tile(coord)
{
  // [room] is the room on which depends the opening condition

  frontTexture = Some(new GraphicEntity(Animation.load("door.png", 1), coord, GameWindow.contextGame))
  walkable = false
  seeThrough = false
  flyable = false
  val keyType = "simple key"  // We can specify that a specific key is needed to open the door

  var nextDoor = None       // which door it is linked to in the nextRoom

  def open():Unit = { 
    frontTexture = None
    walkable = true
    seeThrough = true
    flyable = true
    nextDoor match
    {
      case Some(d)  => d.open()
      case _        => ()
    }
  }
  def connectDoor(door:Door):Unit=
  {
    nextDoor = Some(door)
    // We change the position of the top left corner according to the position of the door
    door.room.topLeft.add(coord)
    door.room.topLeft.sub(door.coord)
  }
}

class Receptacle(coord:Point, val room:Room) extends Tile(coord)
{
  val itemToPlace = "key" // name of the item to put in the receptacle
  var full = false  // is the [itemToPlace] inside the receptacle

  override def placeItem(droppedItem:Item, from:Option[SentientEntity]):Unit=
  {
    super.placeItem(droppedItem, from)
    full = item.name == itemToPlace
    //TODO: change texture if the good item is in the receptacle
  }
  override def show():Unit=
  {
    super.show()
    if(full)
      ()  // just show the good texture if the receptacle is full
  }

}

object Room
{
  implicit val rw: ReadWriter[Room] =
    readwriter[ujson.Value].bimap[Room](
      e => ujson.Arr(),
      json => create(json)
    )

  def create(json:ujson.Value):Room=
  {
    return new Room()
  }
}

case class Room()
{
  var topLeftPos = new Point(0, 0)  // coordinates of the top left corner of the smallest box containing the room

  var tiles = MapObject[(Int, Int), Tile]

  var enemies     = Vector[Enemy]()
  var doors       = Vector[Door]()
  var receptacles = Vector[Receptacle]()
  var otherNPCs   = Vector[SentientEntity]()

  var winCondition = ""

  def create(shape:String, size:MapObject[String, Int], doors:MapObject[(Int, Int), Door], enemies:MapObject[(Int, Int), Enemy], items:MapObject[(Int, Int), Item]):Room =
  {
    // TODO
  }
  def setOffset(ref:Point, p:Point)
  {
    topLeftPos.add(ref)
    topLeftPos.sub(p)
    tiles.foreach
    {
      case (key, value) => value.setOffset(topLeftPos)
    }
  }
  def unlock():Unit = 
  {
    doors.foreach{ d => d.open() }
  }
  def checkWin():Unit =
  {
    // check if winning conditions is verified
    if (winCondition == "killAll" && enemies.length == 0)
      unlock()
    else if (winCondition == "receptacles" && checkReceptacles())
      unlock()
  }
  def checkReceptacles():Boolean=
  {
    var result = true
    receptacles.foreach( e => e.full && result )
  }

  def update():Unit=
  {
    enemies.filter(_.curHP <= 0)
    receptacles.filter(_.full)
    checkWin()
  }
  def setHighlight(zone:(Point=>Boolean), attackHighlight:Boolean=false, erase:Boolean=true, highlightPlayer:Boolean=false):Unit =
  {
        tiles.foreach
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
  }
}
