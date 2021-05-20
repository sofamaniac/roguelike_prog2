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
    val highlightAttack:GraphicEntity = new GraphicEntity(new Animation("highlightAttackTexture.png", 1), new Point(0,0), GameWindow.contextGame)
    val highlight:GraphicEntity = new GraphicEntity(new Animation("highlightTexture.png", 1), new Point(0, 0), GameWindow.contextGame)
    val seen:GraphicEntity      = new GraphicEntity(new Animation("seenTexture.png", 1), new Point(0, 0), GameWindow.contextGame)


    def show(texture:GraphicEntity, pos:Point)
    {
      texture.pos.setPoint(pos)
      texture.show()
    }
}

object Tile
{
  implicit val rw: ReadWriter[Tile] =
    readwriter[ujson.Value].bimap[Tile](
      e => write(e),
      json => load(json)
    )

  def write(t:Tile):ujson.Value =
  {
    var res = "{" 
        res += s""""coord":${upickle.default.write(t.coord)},""""
        res += s"""item":${upickle.default.write(t.item)},""""
        res += s"""entity":${upickle.default.write(t.entity)},""""
        res += s"""walkable":${upickle.default.write(t.walkable)},""""
        res += s"""seeThrough":${upickle.default.write(t.seeThrough)},""""
        res += s"""flyable":${upickle.default.write(t.flyable)},""""
        res += s"""seen":${upickle.default.write(t.seen)},""""
        res += s"""backTexture":${upickle.default.write(t.backTexture)},""""
        res += s"""frontTexture":${upickle.default.write(t.frontTexture)}""""
        res += s"}"
    return res
  }
  def load(json:ujson.Value):Tile =
  {
    val tile = new Tile(upickle.default.read[Point](json("coord")))
    tile.item = upickle.default.read[Option[Item]](json("item"))
    tile.entity = upickle.default.read[Option[SentientEntity]](json("entity"))
    tile.walkable = upickle.default.read[Boolean](json("walkable"))
    tile.seeThrough = upickle.default.read[Boolean](json("seeThrough"))
    tile.seen = upickle.default.read[Boolean](json("seen"))
    tile.backTexture = upickle.default.read[GraphicEntity](json("backTexture"))
    tile.frontTexture = upickle.default.read[Option[GraphicEntity]](json("frontTexture"))
    return tile
  }
}

case class Tile(val coord:Point)
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

    var backTexture:GraphicEntity = new GraphicEntity(Animation.load("texture.png", 1), coord, GameWindow.contextGame)

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
  frontTexture = Some(new GraphicEntity(new Animation("wall.png", 1), coord, GameWindow.contextGame))
  walkable = false
  seeThrough = false
  flyable = false
}

object Map 
{

    var map = new Map

    implicit val rw: ReadWriter[Map] =
      readwriter[ujson.Value].bimap[Map](
        m => write(m),
        json => read(json)
    )

    def write(m:Map):ujson.Value=
    {
      var res = ""
      m.rooms.foreach { case ((x,y), r) => res = res + s"[[${x}, ${y}], ${upickle.default.write(r)}]," } 
      return """{ "rooms": [""" + s"""${JsonTools.sanitize(res)}""" + " ]}"
    }

    def read(_json:ujson.Value):Map =
    {
      val map = new Map()
      val json = ujson.read(_json)
      map.rooms = MapObject[(Int, Int), Room]()
      /*
      JsonTools.foreach(json("rooms"), ( j => println(upickle.default.read[(Int, Int)](j(0)))))
      map.rooms = upickle.default.read[MapObject[(Int, Int), Room]](json("rooms"))
      */
      return map
    }

    def show() = map.show()
    /** Update all the rooms of the map */
    def update() = map.update()

    def setHighlight(zone:(Point=>Boolean), attackHighlight:Boolean=false, erase:Boolean=true, highlightPlayer:Boolean=false):Unit =
      map.setHighlight(zone, attackHighlight, erase, highlightPlayer)

    def findHighlight():Point =
      map.findHighlight()

    def inSight(source:Point, dest:Point):Boolean =
      map.inSight(source, dest)

    def fromPoint(p:Point):Tile =
      map.fromPoint(p)

    def isInbound(p:Point):Boolean =
      map.isInbound(p)

    def getEnemies():Vector[Enemy] =
      map.getEnemies()
}

class Map extends Serializable 
{
    var rooms = MapObject[(Int, Int), Room]()

    rooms += (0,0) -> RoomCreator.create("room2")
    //rooms += (0,1) -> RoomCreator.create("room2")
    //rooms += (1,0) -> RoomCreator.create("room3")

    //rooms((0,0)).tiles((5,13)).asInstanceOf[Door].connectDoor(rooms((0,1)).tiles((5,0)).asInstanceOf[Door])
    //rooms((0,0)).tiles((13,5)).asInstanceOf[Door].connectDoor(rooms((1,0)).tiles((0,10)).asInstanceOf[Door])

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
        case(key, room) => result = result ++ room.enemies ++ room.otherNPCs.asInstanceOf[Vector[Enemy]]
      }
      return result
    }
}

object Door
{
  implicit val rw: ReadWriter[Door] =
    readwriter[ujson.Value].bimap[Door](
      d => "",
      json => upickle.default.read[Door](json)
    )
}
class Door(coord:Point, val room:Room, val openCondition:String) extends Tile(coord)
{
  // [room] is the room on which depends the opening condition

  frontTexture = Some(new GraphicEntity(new Animation("door.png", 1), coord, GameWindow.contextGame))
  walkable = false
  seeThrough = false
  flyable = false
  val keyType = "simple key"                      // We can specify that a specific key is needed to open the door

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

object Receptacle
{
  implicit val rw: ReadWriter[Receptacle] =
    readwriter[ujson.Value].bimap[Receptacle](
      d => "",
      json => upickle.default.read[Receptacle](json)
    )
}
class Receptacle(coord:Point, val room:Room, val itemToPlace:String) extends Tile(coord)
{
  // [itemToPlace] is the name of the item to put in the receptacle
  var full = false  // is the [itemToPlace] inside the receptacle
  frontTexture = Some(new GraphicEntity(new Animation("receptacleOff.png", 1), coord, GameWindow.contextGame))

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
      frontTexture = Some(new GraphicEntity(new Animation("receptacleOn.png", 1), coord, GameWindow.contextGame))
  }

}

object Room
{
  implicit val rw: ReadWriter[Room] =
    readwriter[ujson.Value].bimap[Room](
      e => write(e),
      json => create(json)
    )

  var nameToCreate = ""

  def write(r:Room):ujson.Value=
  {
    var res = "{" + s""""topleft":${JsonTools.sanitize(upickle.default.write(r.topLeft))},"""
        res += s""""tiles":${JsonTools.sanitize(upickle.default.write(r.tiles))},"""
        res += s""""doors":${JsonTools.sanitize(upickle.default.write(r.doors))},"""
        res += s""""enemies":${JsonTools.sanitize(upickle.default.write(r.enemies))},"""
        res += s""""npcs":${JsonTools.sanitize(upickle.default.write(r.otherNPCs))},"""
        res += s""""receptacles":${JsonTools.sanitize(upickle.default.write(r.receptacles))},"""
        res += s""""items":${JsonTools.sanitize(upickle.default.write(r.items))}"""
        res += "}"
    return res
  }

  def loadJson(json:ujson.Value):Room =
  {
    val room = new Room()
    room.topLeft = upickle.default.read[Point](json("topLeft"))
    room.tiles = JsonTools.loadMap[(Int, Int), Tile](json("tiles"))
    room.doors = JsonTools.loadVect[Door](json("doors"))
    room.enemies = JsonTools.loadVect[Enemy](json("enemies"))
    room.otherNPCs = JsonTools.loadVect[SentientEntity](json("npcs"))
    room.receptacles = JsonTools.loadVect[Receptacle](json("receptacles"))
    room.items = JsonTools.loadVect[Item](json("items"))
    return room
  }

  def create(_json:ujson.Value):Room=
  {
    var json = _json
    if (JsonTools.length(json) == 0)
    {
      return loadJson(json)
    }
    var index = JsonTools.find(json, "name", nameToCreate)
    if (index == -1)
    {
      println("room.json: name %s not found.".format(nameToCreate))
      json = JsonTools.getRandom(json)
    }
    else
      json = json(index)

    val shape = "square"
    val size = MapObject("x" -> JsonTools.load(json("size"), "x", 10))  // TODO: in the future the shape of a room could depend on several parameters
    val enemies = loadEnemies(json)
    val doors = loadDoors(json)
    val items = loadItems(json)
    val npcs  = loadNPC(json)
    val recep = loadReceptacles(json)
    val result = new Room()
    result.create(shape, size, enemies, doors, items, npcs, recep)
    return result
  }
  def loadEnemies(json:ujson.Value):MapObject[(Int, Int), Enemy]=
  {
    val array = if(JsonTools.contains(json, "enemies")) json("enemies") else ujson.Arr()
    var result = MapObject[(Int, Int), Enemy]()
    var i = 0
    for(i<-0 to JsonTools.length(array)-1)
    {
      val x = JsonTools.load(array(i), "x", 0)
      val y = JsonTools.load(array(i), "y", 0)
      val enemy = EnemyCreator.create(JsonTools.load(array(i), "name", "bat"))

      result += (x, y) -> enemy
    }
    return result

  }
  def loadDoors(json:ujson.Value):MapObject[(Int, Int), String]=
  {
    val array = if(JsonTools.contains(json, "doors")) json("doors") else ujson.Arr()
    var result = MapObject[(Int, Int), String]()
    var i = 0
    for(i<-0 to JsonTools.length(array)-1)
    {
      val x = JsonTools.load(array(i), "x", 0)
      val y = JsonTools.load(array(i), "y", 0)
      val condition = JsonTools.load(array(i), "openCondition", "killAll")

      result += (x, y) -> condition
    }
    return result
  }
  def loadItems(json:ujson.Value):MapObject[(Int, Int), Item]=
  {
    val array = if(JsonTools.contains(json, "items")) json("items") else ujson.Arr()
    var result = MapObject[(Int, Int), Item]()
    var i = 0
    for(i<-0 to JsonTools.length(array)-1)
    {
      val x = JsonTools.load(array(i), "x", 0)
      val y = JsonTools.load(array(i), "y", 0)
      val item = ItemCreator.create(JsonTools.load(array(i), "name", "chainmail helmet"))

      result += (x, y) -> item
    }
    return result
  }
  def loadNPC(json:ujson.Value):MapObject[(Int, Int), SentientEntity]=
  {
    val array = if(JsonTools.contains(json, "npcs")) json("npcs") else ujson.Arr()
    var result = MapObject[(Int, Int), SentientEntity]()
    var i = 0
    for(i<-0 to JsonTools.length(array)-1)
    {
      val x = JsonTools.load(array(i), "x", 0)
      val y = JsonTools.load(array(i), "y", 0)
      val enemy = EnemyCreator.create(JsonTools.load(array(i), "name", "Jean-Michel"))

      result += (x, y) -> enemy
    }
    return result
  }
  def loadReceptacles(json:ujson.Value):MapObject[(Int, Int), String]=
  {
    val array = if(JsonTools.contains(json, "receptacles")) json("receptacles") else ujson.Arr()
    var result = MapObject[(Int, Int), String]()
    var i = 0
    for(i<-0 to JsonTools.length(array)-1)
    {
      val x = JsonTools.load(array(i), "x", 0)
      val y = JsonTools.load(array(i), "y", 0)
      val item = JsonTools.load(array(i), "item", "chainmail helmet")

      result += (x, y) -> item
    }
    return result
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
  var items       = Vector[Item]()

  def create(shape:String, size:MapObject[String, Int], enemy:MapObject[(Int, Int), Enemy], door:MapObject[(Int, Int), String], 
    item:MapObject[(Int, Int), Item], npcs:MapObject[(Int, Int), SentientEntity], recep:MapObject[(Int, Int), String]) =
  {
    var i = 0
    var j = 0
    for (i <- 0 to size("x"); j <- 0 to size("x"))
    {
      if (i == size("x") || j == size("x") || i == 0 || j == 0)
        tiles += (i, j) -> new Wall(new Point(i,j))
      else
        tiles += (i, j) -> new Tile(new Point(i,j))
    }
    enemy.foreach
    {
      case (key, e) =>
        tiles(key).entity = Some(e)
        enemies = enemies :+ e
        e.pos.setPoint(new Point(key._1, key._2))
    }
    door.foreach
    {
      case(key, d) =>
        tiles(key) = new Door(new Point(key._1, key._2), this, d)
        doors = doors :+ tiles(key).asInstanceOf[Door]
    }
    recep.foreach
    {
      case(key, r) =>
        tiles(key) = new Receptacle(new Point(key._1, key._2), this, r)
        receptacles = receptacles :+ tiles(key).asInstanceOf[Receptacle]
    }
    npcs.foreach
    {
      case(key, n) =>
        tiles(key).entity = Some(n)
        otherNPCs = otherNPCs :+ n
        n.pos.setPoint(new Point(key._1, key._2))
    }
    item.foreach
    {
      case(key, i) =>
        tiles(key).item = Some(i)
        items = items :+ i
    }
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
              
                if (zone(value.coord) && value.isVisible() && !value.isInstanceOf[Wall] && Map.inSight(Game.player.pos, new Point(value.coord)))
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
