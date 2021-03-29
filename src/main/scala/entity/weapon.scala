package weapon


import json._
import upickle.default._

import position._
import item._
import entity._
import position.Zones
import map.Map
import messageHandler._


object Weapon{
  implicit val rw: ReadWriter[Weapon] =
    readwriter[ujson.Value].bimap[Weapon](
      e => ujson.Arr(),
      json => create(json)
    )

  // We define the default value for a weapon
  val defName = ""
  val defDescription = "That's a nice item you got here\nif only you knew what it does"
  val defPrice = 0
  val defRarity = 0
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

  def create(json:ujson.Value):Weapon =
  {

    val name = JsonTools.load(json, "name", defName)
    val description = JsonTools.load(json, "description", defDescription)
    val price = JsonTools.load(json, "price", defPrice)
    val rarity = JsonTools.load(json, "rarity", defRarity)
    val modif = JsonTools.load(json, "modif", defModif)
    val zone = loadZone(json) // TODO
    val inRange = JsonTools.load(json, "innerRange", defInR)
    val outRange = JsonTools.load(json, "outerRange", defOutR)
    val range = JsonTools.load(json, "range", defRange)
    val nbRoll = JsonTools.load(json, "nbRoll", defNbRoll)
    val damRoll = JsonTools.load(json, "damRoll", defDamRoll)

    val fireDur = JsonTools.load(json, "fireDuration", defFireDur)
    val fireDam = JsonTools.load(json, "fireDammage", defFireDam)
    val poisDur = JsonTools.load(json, "poisonDuration", defPoisDur)
    val poisDam = JsonTools.load(json, "poisonDamage", defPoisDam)
    val frozDur = JsonTools.load(json, "frozenDuration", defFrozDur)
    val frozDam = JsonTools.load(json, "frozenDamage", defFrozDam)
    val paraDur = JsonTools.load(json, "paralyzedDuration", defParaDur)
    val paraDam = JsonTools.load(json, "paralyzedDamage", defParaDam)

    val vampirism = JsonTools.load(json, "vampirism", defVampirism)

    return new Weapon(name, description, price, rarity, modif, zone, inRange, outRange, range, nbRoll, damRoll,
                      fireDur, fireDam, poisDur, poisDam, frozDur, frozDam, paraDur, paraDam, vampirism)
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
class Weapon(val name:String, val description:String,  val price:Int, val rarity:Int, val modif:String, val zone:Zones.definition,
             val innerRange:Int, val outerRange:Int, val range:Int, val numberRoll:Int, val damageRoll:Int,
             val fireDuration:Int, val fireDamage:Int, val poisonDuration:Int, val poisonDamage:Int,
             val frozenDuration:Int, val frozenDamage:Int, val paralyzedDuration:Int, val paralyzedDamage:Int, val vampirism:Int) extends Item
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

        // TODO:change to use a function in Map or a foreach
        
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
