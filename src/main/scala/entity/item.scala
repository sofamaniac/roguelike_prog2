package item

import entity._
import graphics._
import position._
import map._
import game._

abstract class Item
    extends Entity(AnimationLoader.load("item.png", 1), new Point(0,0), GameWindow.contextGame)
{
  val name:String
    
  val price:Int
  val rarity:Int

  def getInfo():String =
  {
    return "%s".format(name)
  }
  def onUse(user:SentientEntity)
}

// outerRange à 0 pour les sorts qui ne translatent pas
class Weapon(val name:String, val price:Int, val rarity:Int, val modif:String, val zone:((Weapon, Int, Point, Point)=>Boolean), val innerRange:Int, val outerRange:Int, val range:Int, val numberRoll:Int, val damageRoll:Int) extends Item
{
    def move(dir:Point) = {}
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
                    MessageHandler.addInfo("%s hit %s and dealt %d damage.".format(attacker.name, e.name, dmg))
                }
                else
                {
                    MessageHandler.addInfo("%s missed.".format(attacker.name))
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
                if (zone(this, dir, attacker.pos, Map.tileArray(i)(j).coord))
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
