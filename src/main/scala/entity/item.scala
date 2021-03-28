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
      //case "weapon"   => read[Weapon](json)
      case "scroll"   => read[Scroll](json)
      case "grimoire" => read[Grimoire](json)
      case "key"      => read[Key](json)
      case "bandages" => read[Bandages](json)
      case "armor"    => read[Armor](json)
    }
    // TODO: handle common fields
    return result
  }
}

abstract class Item()
    extends Entity(Animation.load("item.png", 1), new Point(0,0), GameWindow.contextGame)
{
  val name:String
    
  val price:Int
  val rarity:Int

  def getInfo():String =
  {
    return "%s".format(name)
  }
  def move(dir:Point) = {}
  def onUse(user:SentientEntity)
}

// outerRange à 0 pour les sorts qui ne translatent pas
class Weapon(val name:String, val price:Int, val rarity:Int, val modif:String, val zone:((Weapon, Int, Point, Point)=>Boolean), val innerRange:Int, val outerRange:Int, val range:Int, val numberRoll:Int, val damageRoll:Int) extends Item
{
    def roll(max:Int=100) = 
    {
        1 + scala.util.Random.nextInt(max)
    }
    def _attack(dest:Point, attacker:SentientEntity, bonus:Int) =
    {
        Map.fromPoint(dest).entity match
        {
            case None => ()
            case Some(e) =>
                if (roll() >= e.armorClass && !e.dodge())
                {
                    var dmg = bonus
                    var i = 0
                    for(i<-1 to numberRoll)
                    {
                        dmg += roll(damageRoll)
                    }
                    e.damage(dmg, attacker)
                    MessageHandler.genInfo.addMessage("%s hit %s and dealt %d damage.".format(attacker.name, e.name, dmg))
                }
                else
                {
                    MessageHandler.genInfo.addMessage("%s missed.".format(attacker.name))
                }
        }
    }
    def attack(dest:Point, attacker:SentientEntity, dir:Int) =

    {
        val bonus:Int = modif match
        {
            case "str" => attacker.baseStr + attacker.modifStr
            case "dex" => attacker.baseDex + attacker.modifDex
            case "pow" => attacker.basePow + attacker.modifPow
        }
        
        for (i<-0 until Map.tileArray.size)
        {
            for(j<-0 until Map.tileArray(i).size)
            {
                if (zone(this, dir, attacker.pos, Map.tileArray(i)(j).coord) && !attacker.pos.equals(new Point(i,j)))
                {
                    _attack(Map.tileArray(i)(j).coord, attacker, bonus/10)
                }
            }
        }
    }
    def onUse(owner:SentientEntity):Unit =
    {
      owner.inventory.add(owner.weapon)
      owner.weapon = this
      owner.inventory.remove(this)
    }
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
  val price = 0
  val rarity = 0
  def onUse(user:SentientEntity) =
  {
    user.curHP = user.maxHP
    user.inventory.remove(this)
  }
}
