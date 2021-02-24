package entity

import graphics._
import scalafx.Includes._
import scalafx.scene.image._
import scalafx.scene.canvas._
import position._
import map._
import game._

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
        val nextX = pos.x + dir.x
        val nextY = pos.y + dir.y
        if (nextX >= 0 && nextY >= 0 && nextX < Map.tileArray.size && nextY < Map.tileArray(nextX).size)
        {
          Map.tileArray(pos.x)(pos.y).entity = None
          pos.add(dir)
          Map.tileArray(pos.x)(pos.y).entity = Some(this)
        }
    }

    def attack()
    def dodge()
    def speak()
    def loot() // Generate loot on death
}

class Player(dest:GraphicsContext)
    extends SentientEntity(AnimationLoader.load("character.png", 4, sizeY=32), new Point(0,0), dest:GraphicsContext)
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
    val arrow = new GraphicEntity(AnimationLoader.load("arrow.png",1), pos, GameWindow.contextGame)


    val dirArray = Array(new Point(1, 0), new Point(0, 1), new Point(-1, 1), new Point(-1, 0), new Point(0, -1), new Point(1, -1))
    var currentDir = 0

    Map.tileArray(pos.x)(pos.y).entity = Some(this)

    override def show() =
    {
      super.show()
      arrow.show()
    }


    def rotate(rot:Int) = 
    {
      currentDir = (currentDir + rot + dirArray.size) % dirArray.size
      arrow.animation(0).setRotate((arrow.animation(0).getRotate() + 60*rot)%360)
    }

    def getDir(dir: Int):Point = 
    {
      dir match
      {
        case 1  => return dirArray(currentDir)
        case -1 => return dirArray((currentDir + 3) % dirArray.size)
      }
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
    def dodge() = {}
}
  
/*
class Chest(dest:GraphicsContext)
    extends SentientEntity(AnimationLoader.load("ressources/chest_animation", 1), new Point(0,0), dest:GraphicsContext)
{
    def loot() =
    {
        
    }
}
*/
