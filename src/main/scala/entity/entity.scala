package entity

import graphics._
import scalafx.Includes._
import scalafx.scene.image._
import scalafx.scene.canvas._
import position._

abstract class Entity(animation:Array[ImageView], pos:Point, dest:GraphicsContext) 
    extends GraphicEntity(animation:Array[ImageView], pos:Point, dest:GraphicsContext)
{
    val name:String
    def move(dir:Point):Unit
}

abstract class SentientEntity(animation:Array[ImageView], pos:Point, dest:GraphicsContext) 
    extends Entity(animation:Array[ImageView], pos:Point, dest:GraphicsContext)
{
    var maxHp:Int           // health points
    var hp:Int              // current hp
    var armorClass:Int      // AC
    val baseSpd:Int         // Speed in tiles
    var modifSpd:Int
    val baseStr:Int         // starting value
    var modifStr:Int        // relative modifications
    val baseDex:Int 
    var modifDex:Int

    def move(dir:Point):Unit = 
    {
        pos.add(dir)
    }

    def attack()
    def dodge()
    def speak()
    def loot() // Generate loot on death
}

class Player(dest:GraphicsContext)
    extends SentientEntity(AnimationLoader.load_animation("ressources/player_animation"), new Point(0,0), dest:GraphicsContext)
{
    val name = "Player"
    var maxHp = 100
    var hp = 100
    var maxSanity:Int = 100
    var sanity:Int = 100
    var armorClass = 30
    val baseSpd = 5
    var modifSpd = 0
    val baseStr = 100
    var modifStr = 0
    val baseDex = 100
    var modifDex = 0
    var weapon = "insert weapon object"

    def move(dir:Point):Unit = 
    {
        pos.add(dir)
    }

    def attack() = 
    {
        // roll 1d100
        // if roll > AC_enemy -> touch
        // roll damage
    }
    def speak() = 
    {
        // free action, once per turn
    }
    def loot() =
    {

    }
}
  
class Chest(dest:GraphicsContext)
    extends SentientEntity(AnimationLoader.load_animation("ressources/chest_animation"), new Point(0,0), dest:GraphicsContext)
{
    def loot() =
    {
        
    }
}