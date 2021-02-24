package item

import entity._
import graphics._
import position._

abstract class Item(val price:Int, val rarity:Int)
    extends Entity(AnimationLoader.load("ressources/default", 1), new Point(0,0), GameWindow.contextGame)
{

}
/*
class MeleeWeapon extends Item
{

}

class RangedWeapon()
{

}

class CasterWeapon()
{
    
}
*/
