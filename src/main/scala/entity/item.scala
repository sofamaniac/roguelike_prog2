package item
import entity._
import graphics._
import position._

abstract class Item extends Entity(AnimationLoader.load("ressource/default", 1), new Point(0, 0), GameWindow.contextGame)

class MeleeWeapon extends Item
{

}

class RangedWeapon extends Item
{

}

class CasterWeapon extends Item
{
    
}
