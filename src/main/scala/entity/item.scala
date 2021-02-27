package item

import entity._
import graphics._
import position._

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
    def move(dir:Point) {}
}

class MeleeWeapon(val name:String, val price:Int, val rarity:Int, val innerRange:Int, val outerRange:Int) extends Weapon(name, price, rarity)
{
    
}

class RangedWeapon(val name:String, val price:Int, val rarity:Int, val innerRange:Int, val outerRange:Int) extends Weapon(name, price, rarity)
{
    
}

class CasterWeapon(val name:String, val price:Int, val rarity:Int, val innerRange:Int, val outerRange:Int) extends Weapon(name, price, rarity)
{
    
}

