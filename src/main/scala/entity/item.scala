package item

import entity._
import graphics._
import position._
import map._

abstract class Item
    extends Entity(AnimationLoader.load("ressources/default", 1), new Point(0,0), GameWindow.contextGame)
{
  val name:String
  val innerRange:Int
  val outerRange:Int
  val damageRoll:Int
  val numberRoll:Int
  def move(dir:Point) = {}
  def attack(dest:Point, str:Int, dex:Int)
  def roll(max:Int=100) = 
  {
    scala.util.Random.nextInt(max)
  }
    
  val price:Int
  val rarity:Int

}

abstract class Weapon(name:String, price:Int, rarity:Int) extends Item
{

  val name = "MeleeWeapon"
  val innerRange = 1
  val outerRange = 1

  val damageRoll = 4
  val numberRoll = 2

  def attack(dest:Point, str:Int, dex:Int) =
  {
    Map.fromPoint(dest).entity match
    {
      case None => ()
      // TODO: if we attack empty tile
      case Some(e) =>
        if (roll() >= e.armorClass && !e.dodge())
        {
          var damage = 0
          var i = 0
          for(i<-0 to numberRoll)
          {
            damage+=roll(damageRoll)
          }
          e.curHP -= damage+(str/10)
        }
    }

  }
  def move(dir:Point) {}
}

class MeleeWeapon(val name:String, val price:Int, val rarity:Int, val innerRange:Int, val outerRange:Int) extends Weapon(name, price, rarity)
{
    
}

  val damageRoll = 4
  val numberRoll = 2

  def attack(dest:Point, str:Int, dex:Int) = {}

class RangedWeapon(val name:String, val price:Int, val rarity:Int, val innerRange:Int, val outerRange:Int) extends Weapon(name, price, rarity)
{
    
}

class CasterWeapon(val name:String, val price:Int, val rarity:Int, val innerRange:Int, val outerRange:Int) extends Weapon(name, price, rarity)
{
    
  val damageRoll = 4
  val numberRoll = 2
  def attack(dest:Point, str:Int, dex:Int) = {}
}

