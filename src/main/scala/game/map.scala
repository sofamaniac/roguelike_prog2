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

import upickle.default._
import json._
import enemy._


object CommonTextures
{
    // Some textures are used by all tiles, so we load them in memory only once
    val highlightAttack:GraphicEntity = new GraphicEntity(Animation.load("highlightAttackTexture.png", 1), new Point(0,0), GameWindow.contextGame)
    val highlight:GraphicEntity = new GraphicEntity(Animation.load("highlightTexture.png", 1), new Point(0, 0), GameWindow.contextGame)
    val seen:GraphicEntity      = new GraphicEntity(Animation.load("seenTexture.png", 1), new Point(0, 0), GameWindow.contextGame)
    val unseen:GraphicEntity    = new GraphicEntity(Animation.load("unseenTexture.png", 1), new Point(0, 0), GameWindow.contextGame)

    def show(texture:GraphicEntity, pos:Point)
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

    var textureMap = MapObject[String, Option[GraphicEntity]]()

    var backTexture:GraphicEntity      = new GraphicEntity(Animation.load("texture.png", 1), coord, GameWindow.contextGame)
    var frontTexture:Option[GraphicEntity] = None
    
    val infoDest = GameWindow.contextMenu

    val textures = CommonTextures

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
        case _       => ()
      }
      entity match
      {
        case Some(e) => e.pos.add(off)
        case _       => ()
      }
    }
    def placeItem(droppedItem:Item, from:Option[SentientEntity]=None):Unit=
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
    var rooms = MapObject[(Int, Int), Room]()
    rooms += (0,0) -> Room.create()
    rooms += (0,1) -> Room.create()

    rooms((0, 0)).tiles((3, 10)).asInstanceOf[Door].connectDoor(rooms((0, 1)).tiles((3, 0)).asInstanceOf[Door])

    /** Display the entirety of the map on the screen */
    def show() = 
    {
        rooms.foreach
        {
            case(key, room) => room.show()
        }
    }

    /** Update all the rooms of the map */
    def update() =
    {
      rooms.foreach
      {
        case(key, room) => room.update()
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

        rooms.foreach
        {
            case(key, r) =>
              r.tiles.foreach
              {
                case(k, t) =>
                  if (t.isHighlighted())
                      return t.coord
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
        // TODO: change to not create a Point for each iteration
        result = result && fromPoint(new Point(x.toInt,y.toInt)).seeThrough
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
        // We look for the room containing the tile at position [p]
        rooms.foreach
        {
            case(key, room) => 
              if (room.contains(p)) 
                return room.load(p)
        }
        return rooms(0,0).tiles(0,0)  // default value
    }

    def isInbound(p:Point):Boolean =
    {
        var result = false
        rooms.foreach
        {
            case(key, room) => result = result || (room.contains(p))
        }
        return result
    }

    def getEnemies():Vector[Enemy] =
    {
      var result = Vector[Enemy]()
      rooms.foreach
      {
        case(key, room) => result = result ++ room.enemies
      }
      return result
    }
}

class Door(coord:Point, val room:Room) extends Tile(coord)
{
  // [room] is the room on which depends the opening condition

  frontTexture = Some(new GraphicEntity(Animation.load("door.png", 1), coord, GameWindow.contextGame))
  walkable = false
  seeThrough = false
  flyable = false
  var openCondition = "key"
  val keyType = ""                      // We can specify that a specific key is needed to open the door

  var nextDoor:Option[Door] = None      // which door it is linked to in the nextRoom

  def open():Unit = { 
    if(walkable)
      return  // The door is already open
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
    door.nextDoor = Some(this)
    // We change the position of the top left corner according to the position of the door
    door.room.setOffset(coord, door.coord)
  }
  def checkOpen(enemies:Vector[Enemy], receptacles:Vector[Receptacle]):Unit=
  {
    openCondition match
    {
      case "key"  => ()
      case "killAll"  => if(enemies.length == 0) open()
      case "receptacles" => if (checkReceptacles(receptacles)) open()
    }
  }
  def checkReceptacles(r:Vector[Receptacle]):Boolean=
  {
    var result = true
    r.foreach
    {
      e => result = e.full && result 
    }
    return result
  }
}

class Receptacle(coord:Point, val room:Room) extends Tile(coord)
{
  var itemToPlace = "key" // name of the item to put in the receptacle
  var full = false  // is the [itemToPlace] inside the receptacle
  frontTexture = Some(new GraphicEntity(Animation.load("receptacleOff.png", 1), coord, GameWindow.contextGame))

  override def placeItem(droppedItem:Item, from:Option[SentientEntity]=None):Unit=
  {
    super.placeItem(droppedItem, from)
    item match
    {
      case Some(i)  => full = i.name == itemToPlace
      case _        => ()
    }
    //TODO: change texture if the good item is in the receptacle
  }
  override def show():Unit=
  {
    super.show()
    if(full)
      frontTexture = Some(new GraphicEntity(Animation.load("receptacleOn.png", 1), coord, GameWindow.contextGame))
  }

}

object Room
{
  implicit val rw: ReadWriter[Room] =
    readwriter[ujson.Value].bimap[Room](
      e => ujson.Arr(),
      json => createJson(json)
    )

  def createJson(json:ujson.Value):Room=
  {
    return new Room()
  }
  def create():Room=
  {
    var room = new Room()
    room.create()
    return room
  }
}

case class Room()
{
  var topLeft = new Point(0, 0)  // coordinates of the top left corner of the smallest box containing the room

  var tiles = MapObject[(Int, Int), Tile]()

  var enemies     = Vector[Enemy]()
  var doors       = Vector[Door]()
  var receptacles = Vector[Receptacle]()
  var otherNPCs   = Vector[SentientEntity]()

  //def create(shape:String, size:MapObject[String, Int], doors:MapObject[(Int, Int), Door], enemies:MapObject[(Int, Int), Enemy], items:MapObject[(Int, Int), Item]):Room =
  def create():Unit=
  {
    // TODO
    var i = 0
    var j = 0
    for (i <- 0 to 10; j <- 0 to 10)
    {
      if (i == 10 || j == 10 || i == 0 || j == 0)
        tiles += (i, j) -> new Wall(new Point(i,j))
      else
        tiles += (i, j) -> new Tile(new Point(i,j))
    }
    tiles((3, 10)) = new Door(new Point(3, 10), this){ openCondition="killAll" }
    tiles((3,  0)) = new Door(new Point(3,  0), this){ openCondition="receptacles" }
    tiles((5,  5)) = new Receptacle(new Point(5, 5), this){ itemToPlace="bandages" }

    doors = doors :+ tiles((3, 10)).asInstanceOf[Door]
    doors = doors :+ tiles((3,  0)).asInstanceOf[Door]

    receptacles = receptacles :+ tiles((5,5)).asInstanceOf[Receptacle]

    enemies = enemies :+ EnemyCreator.create()
    tiles((5,5)).entity = Some(enemies(0))
    enemies(0).pos.setPoint(new Point(5, 5))

    otherNPCs = otherNPCs :+ EnemyCreator.create("Jean-Michel")
    tiles((2,8)).entity = Some(otherNPCs(0))
    otherNPCs(0).pos.setPoint(new Point(2, 8))
  }
  def setOffset(ref:Point, p:Point)
  {
    topLeft.add(ref)
    topLeft.sub(p)
    tiles.foreach
    {
      case (key, value) => value.setOffset(topLeft)
    }
  }
  def checkWin():Unit =
  {
    // We checked for each door if its opening condition is verified
    doors.foreach
    {
      d => d.checkOpen(enemies, receptacles)
    }
  }
  def contains(p:Point):Boolean=
  {
    return tiles contains (p.x - topLeft.x, p.y-topLeft.y)
  }
  def load(p:Point):Tile=
  {
    return tiles((p.x - topLeft.x, p.y-topLeft.y))
  }

  def update():Unit=
  {
    enemies = enemies.filter(_.curHP > 0)
    checkWin()
  }
  def setHighlight(zone:(Point=>Boolean), attackHighlight:Boolean=false, erase:Boolean=true, highlightPlayer:Boolean=false):Unit =
  {
        tiles.foreach
        {
            case(key, value) =>
                value.highlight = value.highlight && !erase     // erase previous highlight
                value.highlightAttack = value.highlightAttack && !erase
              
                if (zone(value.coord) && value.isVisible() && value.walkable && Map.inSight(Game.player.pos, new Point(value.coord)))
                {
                    value.highlight = !attackHighlight
                    value.highlightAttack = attackHighlight
                }
        }
  }
  def show()
  {
    tiles.foreach
    {
      case(key, tile) => tile.show()
    }
  }
}
