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
      case "weapon"   => WeaponCreator.create(json("name").str)
      case "scroll"   => WeaponCreator.create(json("name").str)
      case "grimoire" => WeaponCreator.create(json("name").str)
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
  val weight:Int

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
  val weight = 0
  def onUse(user:SentientEntity) =
  {
    if(Map.fromPoint(Game.cursor.pos).isInstanceOf[Door] && !Map.fromPoint(Game.cursor.pos).walkable
      && Map.fromPoint(Game.cursor.pos).asInstanceOf[Door].keyType == name)
    {
      Map.fromPoint(Game.cursor.pos).asInstanceOf[Door].open()
      user.inventory.remove(this)
    }
  }
}
object Jewel{
  implicit val rw : ReadWriter[Jewel] =
    readwriter[ujson.Value].bimap[Jewel](
      e=> ujson.Arr(e.name, e.price),
      json => new Jewel // TODO differentiate based on armor piece type
    )
}
case class Jewel() extends Item
{
  val name = "simple key"
  val description = ""
  val price = 0
  val rarity = 0
  val weight = 0
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
      json => create(json) // TODO differentiate based on armor piece type
    )

  def create(json:ujson.Value):Armor = {
    json("part").str match
    {
      case "helmet"     => new Helmet
      case "chestplate" => new Chestplate
      case "leggings"   => new Leggings
      case "boots"      => new Boots
    }
  }
}
abstract case class Armor() extends Item
{
  val name = "armor"
  val description = ""
  val price = 0
  val rarity = 0
  val weight = 0
  val armorClass = 0
  // TODO add more modif (fire resistance, ...)
  def onUse(user:SentientEntity):Unit 
  def defend(user:SentientEntity, attacker:SentientEntity):Unit=  // with this method, armor pieces can have effects on their owner and the attacker (eg thorns, heal,...)
  {
  }
}
class Helmet extends Armor 
{
  override val name = "basic helmet"
  def onUse(user:SentientEntity):Unit = 
  {
    user.armorClass = user.armorClass - user.helmet.armorClass
    user.inventory.add(user.helmet)
    user.helmet = this
    user.armorClass = user.armorClass + armorClass
  }

}
class Chestplate extends Armor
{
  override val name = "basic chestplate"

  def onUse(user:SentientEntity):Unit = 
  {
    user.armorClass = user.armorClass - user.chestplate.armorClass
    user.inventory.add(user.chestplate)
    user.chestplate = this
    user.armorClass = user.armorClass + armorClass
  }
}
class Leggings extends Armor
{
  override val name = "basic leggings"

  def onUse(user:SentientEntity):Unit = 
  {
    user.armorClass = user.armorClass - user.helmet.armorClass
    user.inventory.add(user.leggings)
    user.leggings = this
    user.armorClass = user.armorClass + armorClass
  }
}
class Boots extends Armor
{
  override val name = "basic boots"

  override def onUse(user:SentientEntity):Unit = 
  {
    user.armorClass = user.armorClass - user.boots.armorClass
    user.inventory.add(user.boots)
    user.boots = this
    user.armorClass = user.armorClass + armorClass
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
  val weight = 0
  def onUse(user:SentientEntity) =
  {
    user.curHP = user.maxHP
    user.inventory.remove(this)
  }
}
