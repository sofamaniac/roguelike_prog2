package item
import entity._
import graphics._
import position._

abstract class Item extends Entity(AnimationLoader.load("ressource/default", 1), new Point(0, 0), GameWindow.contextGame)

abstract class Weapon extends Item
{
  val innerRange:Int
  val outerRange:Int
  def move(dir:Point) = {}
}

class MeleeWeapon extends Weapon
{
  val name = "MeleeWeapon"
  val innerRange = 1
  val outerRange = 1
}

class RangedWeapon extends Weapon
{
  val name = "RangedWeapon"
  val innerRange = 1
  val outerRange = 3

}

class CasterWeapon extends Weapon
{
  val name = "CasterWeapon"
  val innerRange = 3
  val outerRange = 6
    
}

