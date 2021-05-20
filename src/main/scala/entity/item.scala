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
      e=> JsonTools.write(e),
      json => createItem(json)
    )

  val defName ="item"
  val defDescription = "description"
  val defPrice = 0
  val defRarity = 0
  val defWeight = 0

  var nameToCreate = ""
  def createItem(_json:ujson.Value):Item = {
    var json = _json
    var index = JsonTools.find(json, "name", nameToCreate)
    if (index == -1)
      json = JsonTools.getRandom(json)
    else
      json = json(index)

    val name = JsonTools.load(json, "name", defName)
    val description = JsonTools.load(json, "description", defDescription)
    val price = JsonTools.load(json, "price", defPrice)
    val rarity = JsonTools.load(json, "rarity", defRarity)
    val weight = JsonTools.load(json, "weight", defWeight)

    var result = json("type").str match
    {
      case "weapon"   => WeaponCreator.create(json("name").str)
      case "scroll"   => WeaponCreator.create(json("name").str)
      case "grimoire" => WeaponCreator.create(json("name").str)
      case "key"      => new Key(name, description, price, rarity, weight)
      case "bandages" => new Bandages(name, description, price, rarity, weight)
      case "armor"    => Armor.create(json, name, description, price, rarity, weight)
      case "gem"      => new Gem(name, description, price, rarity, weight)
    }
    return result
  }
}


abstract class Item(override val animation:Animation = new Animation("item.png", 1))
    extends Entity(animation, new Point(0,0), GameWindow.contextGame)
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
  //implicit val rw : ReadWriter[Key] =
    //readwriter[ujson.Value].bimap[Key](
      //e=> ujson.Arr(e.name, e.price),
      //json => new Key
    //)
}
case class Key(val name:String, val description:String, val price:Int, val rarity:Int, val weight:Int) extends Item(new Animation("key.png", 1))
{
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
  //implicit val rw : ReadWriter[Jewel] =
    //readwriter[ujson.Value].bimap[Jewel](
      //e=> ujson.Arr(e.name, e.price),
      //json => new Jewel // TODO differentiate based on armor piece type
    //)
}
case class Jewel(val name:String, val description:String, val price:Int, val rarity:Int, val weight:Int) extends Item(Animation("jewel.png", 1))
{
  def onUse(user:SentientEntity) =
  {
    if(Map.fromPoint(Game.cursor.pos).isInstanceOf[Door] && !Map.fromPoint(Game.cursor.pos).walkable)
    {
      Map.fromPoint(Game.cursor.pos).asInstanceOf[Door].open()
      user.inventory.remove(this)
    }
  }
}
class Gem(val name:String, val description:String, val price:Int, val rarity:Int, val weight:Int) extends Item(new Animation("gem.png", 1))
{
  def onUse(user:SentientEntity)=
  {
  }
}

object Armor{

  def create(json:ujson.Value, name:String, description:String, price:Int, rarity:Int, weight:Int):Armor = {
    val armorClass = JsonTools.load(json, "armorClass", 0)
    JsonTools.load(json, "part", "") match
    {
      case "helmet"     => new Helmet(name, description, price, rarity, weight, armorClass)
      case "chestplate" => new Chestplate(name, description, price, rarity, weight, armorClass)
      case "leggings"   => new Leggings(name, description, price, rarity, weight, armorClass)
      case _            => new Boots(name, description, price, rarity, weight, armorClass)
    }
  }
}
abstract case class Armor()
  extends Item()
{
  val name:String
  val description:String
  val price:Int
  val rarity:Int
  val weight:Int
  val armorClass:Int
  // TODO add more modif (fire resistance, ...)
  def onUse(user:SentientEntity):Unit 
  def defend(user:SentientEntity, attacker:SentientEntity):Unit=  // with this method, armor pieces can have effects on their owner and the attacker (eg thorns, heal,...)
  {
  }
}
class Helmet(val name:String, val description:String, val price:Int, val rarity:Int, val weight:Int, val armorClass:Int)
  extends Armor ()
{
  def onUse(user:SentientEntity):Unit = 
  {
    user.armorClass = user.armorClass - user.helmet.armorClass
    user.inventory.remove(this)
    user.inventory.add(user.helmet)
    user.helmet = this
    user.armorClass = user.armorClass + armorClass
  }

}
class Chestplate(val name:String, val description:String, val price:Int, val rarity:Int, val weight:Int, val armorClass:Int)
  extends Armor ()
{
  def onUse(user:SentientEntity):Unit = 
  {
    user.armorClass = user.armorClass - user.chestplate.armorClass
    user.inventory.remove(this)
    user.inventory.add(user.chestplate)
    user.chestplate = this
    user.armorClass = user.armorClass + armorClass
  }
}
class Leggings(val name:String, val description:String, val price:Int, val rarity:Int, val weight:Int, val armorClass:Int)
  extends Armor ()
{
  def onUse(user:SentientEntity):Unit = 
  {
    user.armorClass = user.armorClass - user.helmet.armorClass
    user.inventory.remove(this)
    user.inventory.add(user.leggings)
    user.leggings = this
    user.armorClass = user.armorClass + armorClass
  }
}
class Boots(val name:String, val description:String, val price:Int, val rarity:Int, val weight:Int, val armorClass:Int)
  extends Armor ()
{
  override def onUse(user:SentientEntity):Unit = 
  {
    user.armorClass = user.armorClass - user.boots.armorClass
    user.inventory.remove(this)
    user.inventory.add(user.boots)
    user.boots = this
    user.armorClass = user.armorClass + armorClass
  }
}

object Bandages{
  //implicit val rw : ReadWriter[Bandages] =
    //readwriter[ujson.Value].bimap[Bandages](
      //e=> ujson.Arr(e.name, e.price),
      //json => new Bandages
    //)
}
case class Bandages(val name:String, val description:String, val price:Int, val rarity:Int, val weight:Int) extends Item()
{
  def onUse(user:SentientEntity) =
  {
    user.curHP = user.maxHP
    user.inventory.remove(this)
  }
}
