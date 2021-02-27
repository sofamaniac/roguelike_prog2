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
    extends GraphicEntity(animation, pos, dest)
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

  def getDir(dir:Int):Point = 
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
    extends Entity(animation, pos, dest)
{
    val name:String
    var maxHP:Int           // health points
    var curHP:Int           // current hp

    var armorClass:Int      // AC
    
    var baseAP:Int          // Action Points
    var modifAP:Int
    var curAP:Int
    
    var baseStr:Int         // starting value
    var modifStr:Int        // relative modifications
    
    var baseDex:Int 
    var modifDex:Int

    var weapon:Weapon       // equipped weapon

    def move(next:Point):Unit = 
    {
      if (isMoveValid(next))
      {
        Map.fromPoint(next).entity = None
        Map.fromPoint(next).entity = Some(this)
        curAP -= pos.distance(next)
        pos.setPoint(next)
      }
    }

    def isMoveValid(next:Point):Boolean =
    {
      return (next.x >= 0 && next.y >= 0 && next.x < Map.tileArray.size && next.y < Map.tileArray(next.y).size
            && Map.tileArray(next.x)(next.y).entity == None && Map.tileArray(next.x)(next.y).walkable)
    }


    def getInfo():String =
    {
      return "%s : %s/%s HP".format(name, curHP, maxHP)
    }

    def attack(dest:Point):Unit =
    {
      weapon.attack(dest, baseStr+modifStr, baseDex+modifDex)
    }

    def dodge():Boolean
    def speak()

    def loot() // Generate loot on death
}


class Cursor(dest:GraphicsContext)
  extends ControlledEntity(AnimationLoader.load("cursor.png", 1), new Point(0,0), dest)
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

  def setPos(dest:Point)
  {
    Map.tileArray(pos.x)(pos.y).selected = false
    pos.setPoint(dest)
    Map.tileArray(pos.x)(pos.y).selected = true
  }

}

class Player(dest:GraphicsContext)
    extends SentientEntity(AnimationLoader.load("character.png", 4, sizeY=64), new Point(0,0), dest)
{
    val name = "Player"

    var maxHP = 100
    var curHP = 100
    
    var maxSanity = 100
    var sanity = 100
    
    var armorClass = 30
    
    var baseAP = 5
    var modifAP = 0
    var curAP = baseAP
    
    var baseStr = 100
    var modifStr = 0
    
    var baseDex = 100
    var modifDex = 0
    
    var seeRange = 5
    var modifSee = 0


    var weapon:Weapon = new CasterWeapon("FireBall", 0, 0, 2, 4, 4, 2)

    def attack()
    {
        // roll 1d100
        // if roll > AC_enemy -> touch
        // roll damage
    }
    def speak()
    {
        // free action, once per turn
    }
    def loot()
    {

    }

    def dodge():Boolean = {return false}

    def getSeeRange():Int = 
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
