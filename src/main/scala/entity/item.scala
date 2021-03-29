package item

import entity._
import graphics._
import messageHandler._
import animation._
import position._
import map._
import game._

import upickle.default._
import json._

import weapon._

object Item {

  implicit val rw: ReadWriter[Item] = 
    readwriter[ujson.Value].bimap[Item](
      e=> ujson.Arr(e.name, e.price),
      json => createItem(json)
    )

  var nameToCreate = ""
  def createItem(_json:ujson.Value):Item = {
    var json = _json
    var index = JsonTools.find(json, "name", nameToCreate)
    if (index == -1)
      json = JsonTools.getRandom(json)
    else
      json = json(index)

    var result = json("type").str match
    {
      case "weapon"   => read[Weapon](json)
      case "scroll"   => read[Scroll](json)
      case "grimoire" => read[Grimoire](json)
      case "key"      => read[Key](json)
      case "bandages" => read[Bandages](json)
      case "armor"    => read[Armor](json)
    }
    return result
  }
}

abstract class Item()
    extends Entity(Animation.load("item.png", 1), new Point(0,0), GameWindow.contextGame)
{
  val name:String
  val description:String
    
  val price:Int
  val rarity:Int

  def getInfo():String =
  {
    return "%s".format(name)
  }

  def getDescription():String =
  {
    return "%s".format(description)
  }
  def move(dir:Point) = {}
  def onUse(user:SentientEntity)
}



object Key{
  implicit val rw : ReadWriter[Key] =
    readwriter[ujson.Value].bimap[Key](
      e=> ujson.Arr(e.name, e.price),
      json => new Key
    )
}
case class Key() extends Item
{
  val name = "simple key"
  val description = ""
  val price = 0
  val rarity = 0
  def onUse(user:SentientEntity) =
  {
    if(Map.fromPoint(Game.cursor.pos).isInstanceOf[Door] && !Map.fromPoint(Game.cursor.pos).walkable)
    {
      Map.fromPoint(Game.cursor.pos).asInstanceOf[Door].open()
      user.inventory.remove(this)
    }
  }
}

object Armor{
  implicit val rw : ReadWriter[Armor] =
    readwriter[ujson.Value].bimap[Armor](
      e=> ujson.Arr(e.name, e.price),
      json => new Armor
    )
}
case class Armor() extends Item
{
  val name = "armor"
  val description = ""
  val price = 0
  val rarity = 0
  def onUse(user:SentientEntity) =
  {
    if(Map.fromPoint(Game.cursor.pos).isInstanceOf[Door] && !Map.fromPoint(Game.cursor.pos).walkable)
    {
      Map.fromPoint(Game.cursor.pos).asInstanceOf[Door].open()
      user.inventory.remove(this)
    }
  }
}

object Scroll{
  implicit val rw : ReadWriter[Scroll] =
    readwriter[ujson.Value].bimap[Scroll](
      e=> ujson.Arr(e.name, e.price),
      json => new Scroll
    )
}
case class Scroll() extends Item
{
  val name = "scroll"
  val description = ""
  val price = 0
  val rarity = 0
  def onUse(user:SentientEntity) =
  {
    if(Map.fromPoint(Game.cursor.pos).isInstanceOf[Door] && !Map.fromPoint(Game.cursor.pos).walkable)
    {
      Map.fromPoint(Game.cursor.pos).asInstanceOf[Door].open()
      user.inventory.remove(this)
    }
  }
}

object Grimoire{
  implicit val rw : ReadWriter[Grimoire] =
    readwriter[ujson.Value].bimap[Grimoire](
      e=> ujson.Arr(e.name, e.price),
      json => new Grimoire
    )
}
case class Grimoire() extends Item
{
  val name = "grimoire"
  val description = ""
  val price = 0
  val rarity = 0
  def onUse(user:SentientEntity) =
  {
    if(Map.fromPoint(Game.cursor.pos).isInstanceOf[Door] && !Map.fromPoint(Game.cursor.pos).walkable)
    {
      Map.fromPoint(Game.cursor.pos).asInstanceOf[Door].open()
      user.inventory.remove(this)
    }
  }
}

object Bandages{
  implicit val rw : ReadWriter[Bandages] =
    readwriter[ujson.Value].bimap[Bandages](
      e=> ujson.Arr(e.name, e.price),
      json => new Bandages
    )
}
case class Bandages() extends Item
{
  val name = "bandages"
  val description = ""
  val price = 0
  val rarity = 0
  def onUse(user:SentientEntity) =
  {
    user.curHP = user.maxHP
    user.inventory.remove(this)
  }
}
