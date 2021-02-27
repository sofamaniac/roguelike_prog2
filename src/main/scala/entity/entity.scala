package entity

import graphics._
import scalafx.Includes._
import scalafx.scene.image._
import scalafx.scene.canvas._
import position._
import map._
import game._
import item._

abstract class Entity(animation:Array[ImageView], pos:Point, dest:GraphicsContext) 
    extends GraphicEntity(animation:Array[ImageView], pos:Point, dest:GraphicsContext)
{
    val name:String
    def move(dir:Point):Unit
}

abstract class ControlledEntity(animation:Array[ImageView], pos:Point, dest:GraphicsContext)
  extends Entity(animation, pos, dest)
{
  val arrow = new GraphicEntity(AnimationLoader.load("arrow.png", 6), pos, dest)
  arrow.freeze()

  val dirArray = Array(new Point(1, 0), new Point(0, 1), new Point(-1, 1), new Point(-1, 0), new Point(0, -1), new Point(1, -1))
  var currentDir = 0

  def rotate(rot:Int) = 
  {
    currentDir = (currentDir + rot + dirArray.size) % dirArray.size
    arrow.currentFrame = (arrow.currentFrame + rot + dirArray.size) % dirArray.size
  }

  def getDir(dir: Int):Point = 
  {
    dir match
    {
      case 1  => return dirArray(currentDir)
      case -1 => return dirArray((currentDir + 3) % dirArray.size)
    }
  }

  def move(dir:Point):Unit = 
  {
    val nextX = pos.x + dir.x
    val nextY = pos.y + dir.y
    if (nextX >= 0 && nextY >= 0 && nextX < Map.tileArray.size && nextY < Map.tileArray(nextX).size)
    {
      pos.add(dir)
    }
  }
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
    
    var maxAP:Int           // action points
    var curAP:Int

    override def move(next:Point):Unit = 
    {
      val nextX = next.x
      val nextY = next.y
      if (nextX >= 0 && nextY >= 0 && nextX < Map.tileArray.size && nextY < Map.tileArray(nextX).size)
      {
        Map.tileArray(pos.x)(pos.y).entity = None
        Map.tileArray(nextX)(nextY).entity = Some(this)
        pos.setPoint(next)
      }
    }

    def getInfo():String =
    {
      return "%s : %s/%s HP".format(name, hp, maxHp)
    }

    def attack()
    def dodge()
    def speak()
    def loot() // Generate loot on death
}


class Cursor(dest:GraphicsContext)
  extends ControlledEntity(AnimationLoader.load("cursor.png", 1), new Point(0,0), dest:GraphicsContext)
{
  val name = "cursor"
  var limitation = false  // indicates if the cursor is restricted to highlighted tiles

  override def show() = 
  {
    arrow.show()
    super.show()
  }

  override def move(dir:Point) =
  {
    val nextX = pos.x + dir.x
    val nextY = pos.y + dir.y
    if (nextX >= 0 && nextY >= 0 && nextX < Map.tileArray.size && nextY < Map.tileArray(nextX).size
        && Map.tileArray(nextX)(nextY).isVisible() && (!limitation || Map.tileArray(nextX)(nextY).highlight))
    {
      Map.tileArray(pos.x)(pos.y).selected = false
      pos.add(dir)
      Map.tileArray(pos.x)(pos.y).selected = true
    }
  }

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
    val seeRange = 5
    var modifSee = 0

    var maxAP = 5
    var curAP = 5

    var weapon = new CasterWeapon

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

    def getSeeRange() : Int = 
    {
      return seeRange + modifSee
    }
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
