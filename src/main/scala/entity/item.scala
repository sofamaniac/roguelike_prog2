package item

abstract class Item(val price:Int, val rarity:Int)
    extends Entity(AnimationLoader.load("ressources/item_base", 1), new Point(0,0), dest:GraphicsContext)
{

}

class MeleeWeapon()
    extends Item()
{

}

class RangedWeapon()
{

}

class CasterWeapon()
{
    
}