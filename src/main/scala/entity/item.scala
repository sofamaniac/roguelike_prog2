package item

import entity._
import graphics._
import position._
import map._

abstract class Item
    extends Entity(AnimationLoader.load("ressources/default", 1), new Point(0,0), GameWindow.contextGame)
{
  val name:String
    
  val price:Int
  val rarity:Int
}

abstract class Weapon(name:String, price:Int, rarity:Int) extends Item 
{
  val innerRange:Int
  val outerRange:Int
  val damageRoll:Int
  val numberRoll:Int
  def move(dir:Point) = {}
  def attack(dest:Point, attacker:SentientEntity)
  def roll(max:Int=100) = 
  {
    1 + scala.util.Random.nextInt(max)
  }
  def _attack(dest:Point, attacker:SentientEntity, bonus:Int) =
  {
    Map.fromPoint(dest).entity match
    {
        case None => ()
        // TODO: if we attack empty tile
        case Some(e) =>
            if (roll() >= e.armorClass && !e.dodge())
            {
                var dmg = 0
                var i = 0
                for(i<-1 to numberRoll)
                {
                    dmg += roll(damageRoll)
                }
                e.damage(dmg + bonus, attacker)
                MessageHandler.addInfo("Hit %s and dealt %d damage.".format(e.name, dmg))
            }
            else
            {
              MessageHandler.addInfo("Missed.")
            }
        }
    }
}

class MeleeWeapon(val name:String, val price:Int, val rarity:Int, val innerRange:Int, val outerRange:Int, val numberRoll:Int, val damageRoll:Int) extends Weapon(name, price, rarity)
{
  def attack(dest:Point, attacker:SentientEntity) =
  {
    _attack(dest, attacker, (attacker.baseStr + attacker.modifStr)/10)
  }
}

class RangedWeapon(val name:String, val price:Int, val rarity:Int, val innerRange:Int, val outerRange:Int, val numberRoll:Int, val damageRoll:Int) extends Weapon(name, price, rarity)
{
    def attack(dest:Point, attacker:SentientEntity) = 
    {
        _attack(dest, attacker, (attacker.baseDex + attacker.modifDex)/10)
    }
}

class CasterWeapon(val name:String, val price:Int, val rarity:Int, val innerRange:Int, val outerRange:Int, val numberRoll:Int, val damageRoll:Int) extends Weapon(name, price, rarity)
{

  def attack(dest:Point, attacker:SentientEntity) = 
  {
    //attack each tile in the AoE separately ?
  }
}

