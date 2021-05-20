package weapon

import json._
import upickle.default._

import position._
import item._
import entity._
import position.Zones
import map.Map
import messageHandler._
import game._
import animation._
import scala.collection.mutable.{Map => MapObject}

// We simulate type disjunction
// arguments for a weapon can be of three types only : Integer, String or Zone
sealed trait ArgsType{ 
  def int():Int  =    { return 0 }
  def str():String =  { return ""}
}
case class Num(val i:Int) extends ArgsType{ override def int():Int = {return i}}
case class Str(val s:String) extends ArgsType{ override def str():String = {return s}}

object Weapon{
  implicit val rw: ReadWriter[Weapon] =
    readwriter[ujson.Value].bimap[Weapon](
      e => JsonTools.write(e),
      json => create(json)
    )

  // We define the default value for a weapon
  val defName = ""
  val defDescription = "That's a nice item you got here\nif only you knew what it does"
  val defType = "weapon"
  val defPrice = 0
  val defRarity = 0
  val defWeight = 0

  val defModif = "str"
  val defZone = "classic"
  val defInR = 0
  val defOutR = 5
  val defRange = 5
  val defNbRoll = 1
  val defDamRoll = 8

  val defFireDur = 0
  val defFireDam = 0
  val defPoisDur = 0
  val defPoisDam = 0
  val defFrozDur = 0
  val defFrozDam = 0
  val defParaDur = 0
  val defParaDam = 0

  val defVampirism = 0 // Integer between 0 and 100 representing the proportion of damage made that the attacker gets added to its HP

  val defCost = 0
  val defMinPow = 0
  
  var nameToCreate = ""
  def create(_json:ujson.Value):Weapon =
  {
    var json = _json
    var index = JsonTools.find(json, "name", nameToCreate)
    if (index == -1)
      json = JsonTools.getRandom(json)
    else
      json = json(index)

    var args = MapObject[String, ArgsType]()

    args += "name"        -> Str(JsonTools.load(json, "name", defName))
    args += "description" -> Str(JsonTools.load(json, "description", defDescription))
    args += "price"       -> Num(JsonTools.load(json, "price", defPrice))
    args += "rarity"      -> Num(JsonTools.load(json, "rarity", defRarity))
    args += "weight"      -> Num(JsonTools.load(json, "weight", defWeight))
    args += "modif"       -> Str(JsonTools.load(json, "modif", defModif))
    args += "zone"        -> Str(JsonTools.load(json, "zone", defZone))
    args += "innerRange"  -> Num(JsonTools.load(json, "innerRange", defInR))
    args += "outerRange"  -> Num(JsonTools.load(json, "outerRange", defOutR))
    args += "numberRoll"  -> Num(JsonTools.load(json, "nbRoll", defNbRoll))
    args += "damageRoll"  -> Num(JsonTools.load(json, "damRoll", defDamRoll))

    args += "fireDuration"      -> Num(JsonTools.load(json, "fireDuration", defFireDur))
    args += "fireDamage"        -> Num(JsonTools.load(json, "fireDammage", defFireDam))
    args += "poisonDuration"    -> Num(JsonTools.load(json, "poisonDuration", defPoisDur))
    args += "poisonDamage"      -> Num(JsonTools.load(json, "poisonDamage", defPoisDam))
    args += "frozenDuration"    -> Num(JsonTools.load(json, "frozenDuration", defFrozDur))
    args += "frozenDamage"      -> Num(JsonTools.load(json, "frozenDamage", defFrozDam))
    args += "paralyzedDuration" -> Num(JsonTools.load(json, "paralyzedDuration", defParaDur))
    args += "paralyzedDamage"   -> Num(JsonTools.load(json, "paralyzedDamage", defParaDam))

    args += "vampirism" -> Num(JsonTools.load(json, "vampirism", defVampirism))
    
    val cost = JsonTools.load(json, "spellCost", defCost)   // Only applicable to scrolls and grimoires
    val minPow = JsonTools.load(json, "minPow", defMinPow)  // Only for grimoires

    JsonTools.load(json, "type", defType) match
    {
      case "scroll" =>  new Scroll(args, cost)
      case "grimoire" => new Grimoire(args, cost, minPow)
      case _ =>         new Weapon(args)
    }
  }

  def loadZone(json:ujson.Value):Zones.definition =
  {
    JsonTools.load(json, "zone", defZone) match
    {
      case "ray"      => Zones.ray
      case "cone"     => Zones.cone
      case "ring"     => Zones.ring
      case "classic"  => Zones.classic
      case _          => Zones.classic
    }
  }
}


// outerRange à 0 pour les sorts qui ne translatent pas
class Weapon(val name:String, val description:String,  val price:Int, val rarity:Int, val weight:Int, val modif:String, val zone:String,
             val innerRange:Int, val outerRange:Int, val numberRoll:Int, val damageRoll:Int,
             val fireDuration:Int, val fireDamage:Int, val poisonDuration:Int, val poisonDamage:Int,
             val frozenDuration:Int, val frozenDamage:Int, val paralyzedDuration:Int, val paralyzedDamage:Int, val vampirism:Int) extends Item(new Animation("gun.png", 1))
{
    def this(map:MapObject[String, ArgsType])=
    {
      // Using the ersatz of type disjunction we defined, we can make a much simpler constructor for weapon
      this(map("name").str, map("description").str, map("price").int, map("rarity").int, map("weight").int, map("modif").str, map("zone").str, 
            map("innerRange").int, map("outerRange").int, map("numberRoll").int, map("damageRoll").int, map("fireDuration").int, map("fireDamage").int,
            map("poisonDuration").int, map("poisonDamage").int, map("frozenDuration").int, map("frozenDamage").int,
            map("paralyzedDuration").int, map("paralyzedDamage").int, map("vampirism").int)
    }
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
                    attacker.heal(dmg*vampirism / 100)
                    attacker.addEffects(fireDuration, fireDamage, poisonDuration, poisonDamage, frozenDuration, frozenDamage, paralyzedDuration, paralyzedDamage)
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
        
        Map.map.rooms.foreach
        {
          case(key, r) =>
            r.tiles.foreach
            {
              case (key, value) =>
                if (getZone()(this.innerRange, this.outerRange, dir, attacker.pos, value.coord) && !attacker.pos.equals(value.coord))
                {
                    _attack(value.coord, attacker, bonus/10)
                }
            }
        }
    }
    def onUse(owner:SentientEntity):Unit =
    {
      owner.inventory.add(owner.weapon)
      owner.weapon = this
      owner.inventory.remove(this)
      if(owner == Game.player)
        Game.currentWeapon = this
    }
    def getZone():Zones.definition =
    {
      zone match
      {
        case "ring" => Zones.ring
        case "ray"  => Zones.ray
        case "cone" => Zones.cone
        case _      => Zones.classic
      }
    }
}
abstract class MagicWeapon(args:MapObject[String, ArgsType], val cost:Int)  extends Weapon(args)
{
  override def onUse(owner:SentientEntity):Unit = 
  {
    // Only the player can call this function
    Game.changeWeapon(this)
    Game.setPhase("attack")
  }
  override def attack(dest:Point, attacker:SentientEntity, dir:Int) =
  {
    super.attack(dest, attacker, dir)
    attacker.applyMagicCost(cost)
  }
}

class Scroll(args:MapObject[String, ArgsType], cost:Int)  extends MagicWeapon(args, cost)
{
  override def attack(dest:Point, attacker:SentientEntity, dir:Int) =
  {
    super.attack(dest, attacker, dir)
    attacker.inventory.remove(this)
  }
}

class Grimoire(args:MapObject[String, ArgsType], cost:Int, val minPow:Int)  extends MagicWeapon(args, cost)
{
  override def onUse(owner:SentientEntity):Unit =
  {
    if (owner.basePow + owner.modifPow >= minPow)
      super.onUse(owner)
  }
}
