package item
import entity._
import graphics._
import position._
import map._

abstract class Item extends Entity(AnimationLoader.load("ressource/default", 1), new Point(0, 0), GameWindow.contextGame)

abstract class Weapon extends Item
{
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
}

class MeleeWeapon extends Weapon
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

}

class RangedWeapon extends Weapon
{
  val name = "RangedWeapon"
  val innerRange = 1
  val outerRange = 3

  val damageRoll = 4
  val numberRoll = 2

  def attack(dest:Point, str:Int, dex:Int) = {}

}

class CasterWeapon extends Weapon
{
  val name = "CasterWeapon"
  val innerRange = 3
  val outerRange = 6
    
  val damageRoll = 4
  val numberRoll = 2
  def attack(dest:Point, str:Int, dex:Int) = {}
}

